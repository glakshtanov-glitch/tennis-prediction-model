# compare_backtest.R
# Three-way backtest on 2020-2024: model_inter3 vs model_wform vs SS Rule
# High-odds subset (implied_prob_A < 0.29), edge thresholds 0.05-0.25
# One row per match (deduplicated). Saves backtest_results_comparison.csv

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")

# ── 0. Prep odds_all with date key ────────────────────────────────────────────
# odds_all Date format: "M/D/YYYY"  →  extract year-month as "YYYY-MM"
cat("Prepping odds...\n")

odds_prep <- odds_all %>%
  rename(surface_odds = Surface) %>%
  filter(!is.na(B365W), !is.na(B365L)) %>%
  mutate(
    date_parts   = str_split(Date, "/"),
    odds_month   = map_chr(date_parts, ~ sprintf("%02d", as.integer(.x[1]))),
    odds_year    = map_chr(date_parts, ~ .x[3]),
    date_key     = paste0(odds_year, "-", odds_month),   # "2022-02"
    overround    = 1/B365W + 1/B365L,
    norm_prob_W  = (1/B365W) / overround,
    norm_prob_L  = (1/B365L) / overround
  ) %>%
  select(winner_abbrev = Winner, loser_abbrev = Loser,
         surface_odds, date_key, B365W, B365L, norm_prob_W, norm_prob_L,
         WRank, LRank)

# ── 1. Join helper ─────────────────────────────────────────────────────────────
# Takes a feature data frame with playerA, playerB, surface (factor), tourney_date (YYYYMMDD int)
# Returns it with odds columns, one row per match_id.

join_and_dedup <- function(feat_df) {
  feat_prep <- feat_df %>%
    mutate(
      playerA_abbrev = map_chr(playerA, abbreviate_name),
      playerB_abbrev = map_chr(playerB, abbreviate_name),
      surface_clean  = as.character(surface),
      td_str         = as.character(tourney_date),
      feat_year      = substr(td_str, 1, 4),
      feat_month     = substr(td_str, 5, 6),
      date_key       = paste0(feat_year, "-", feat_month)
    )

  # A = winner side
  join_a_wins <- feat_prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev" = "winner_abbrev",
                      "playerB_abbrev" = "loser_abbrev",
                      "surface_clean"  = "surface_odds",
                      "date_key"       = "date_key"),
               relationship = "many-to-many") %>%
    mutate(playerA_is_winner = TRUE,
           odds_A = B365W, implied_prob_A = norm_prob_W)

  # A = loser side
  join_a_loses <- feat_prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev" = "loser_abbrev",
                      "playerB_abbrev" = "winner_abbrev",
                      "surface_clean"  = "surface_odds",
                      "date_key"       = "date_key"),
               relationship = "many-to-many") %>%
    mutate(playerA_is_winner = FALSE,
           odds_A = B365L, implied_prob_A = norm_prob_L)

  bind_rows(join_a_wins, join_a_loses) %>%
    mutate(outcome_num = as.integer(as.character(outcome))) %>%
    # One row per match — keep lowest implied_prob_A (best underdog odds)
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# ── 2. model_inter3 backtest (2020-2024) ──────────────────────────────────────
cat("Building model_inter3 predictions...\n")

# test_inter: 2020-2024, already has log_rank_ratio + games_dom_x_underdog (rank-based)
# outcome must be factor for tidymodels
test_inter_fac <- test_inter %>%
  mutate(outcome = factor(outcome, levels = c("0","1")))

preds_i3 <- predict(model_inter3, test_inter_fac, type = "prob") %>%
  rename(prob_inter3 = .pred_1) %>%
  bind_cols(test_inter_fac)

# Join with odds
preds_i3_odds <- join_and_dedup(preds_i3) %>%
  mutate(
    model_prob  = prob_inter3,
    edge        = prob_inter3 - implied_prob_A,
    won_bet     = outcome_num == 1
  )

cat("model_inter3 matched rows:", nrow(preds_i3_odds), "\n")
cat("  High-odds (< 0.29):", sum(preds_i3_odds$implied_prob_A < 0.29), "\n")

# ── 3. model_wform backtest (2020-2024) ───────────────────────────────────────
cat("Building model_wform predictions...\n")

feat_wform <- df_features6 %>%
  mutate(
    year           = as.integer(substr(as.character(tourney_date), 1, 4)),
    log_rank_ratio = log(rankB / rankA),
    wform_diff     = wformA - wformB,
    outcome        = factor(outcome, levels = c("0","1"))
  ) %>%
  filter(year >= 2020, !is.na(log_rank_ratio), !is.na(wform_diff), !is.na(surface))

preds_wf <- predict(model_wform, feat_wform, type = "prob") %>%
  rename(prob_wform = .pred_1) %>%
  bind_cols(feat_wform)

preds_wf_odds <- join_and_dedup(preds_wf) %>%
  mutate(
    model_prob = prob_wform,
    edge       = prob_wform - implied_prob_A,
    won_bet    = outcome_num == 1
  )

cat("model_wform matched rows:", nrow(preds_wf_odds), "\n")
cat("  High-odds (< 0.29):", sum(preds_wf_odds$implied_prob_A < 0.29), "\n")

# ── 4. SS Rule (2020-2024) ────────────────────────────────────────────────────
# Rule: bet on playerA when ss_streakA >= 2 AND playerA is underdog (implied_prob_A < 0.5)
cat("Building SS Rule bets...\n")

feat_ss <- df_features6 %>%
  mutate(
    year           = as.integer(substr(as.character(tourney_date), 1, 4)),
    log_rank_ratio = log(rankB / rankA),
    wform_diff     = wformA - wformB,
    outcome        = factor(outcome, levels = c("0","1"))
  ) %>%
  filter(year >= 2020, !is.na(ss_streakA))

# Get model_wform probability for SS bets (as a confidence filter)
preds_ss_model <- predict(model_wform, feat_ss %>% filter(!is.na(log_rank_ratio), !is.na(wform_diff), !is.na(surface)), type = "prob") %>%
  rename(model_prob = .pred_1) %>%
  bind_cols(feat_ss %>% filter(!is.na(log_rank_ratio), !is.na(wform_diff), !is.na(surface)))

preds_ss_odds <- join_and_dedup(preds_ss_model) %>%
  filter(ss_streakA >= 2, implied_prob_A < 0.5) %>%  # underdog on streak
  mutate(
    edge    = model_prob - implied_prob_A,
    won_bet = outcome_num == 1
  )

cat("SS Rule candidates (streak>=2, underdog):", nrow(preds_ss_odds), "\n")
cat("  High-odds (< 0.29):", sum(preds_ss_odds$implied_prob_A < 0.29), "\n")

# ── 5. Threshold sweep helper ─────────────────────────────────────────────────

sweep_thresholds <- function(df, model_label, subset_label = "all",
                              thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25)) {
  map_dfr(thresholds, function(thr) {
    bets <- df %>% filter(edge >= thr)
    if (nrow(bets) == 0) {
      return(tibble(model = model_label, subset = subset_label,
                    threshold = thr, n_bets = 0L,
                    win_rate = NA_real_, total_pnl = NA_real_,
                    roi = NA_real_, avg_odds = NA_real_, avg_edge = NA_real_))
    }
    tibble(
      model     = model_label,
      subset    = subset_label,
      threshold = thr,
      n_bets    = nrow(bets),
      win_rate  = mean(bets$won_bet),
      total_pnl = sum(if_else(bets$won_bet, bets$odds_A - 1, -1)),
      roi       = total_pnl / n_bets,
      avg_odds  = mean(bets$odds_A),
      avg_edge  = mean(bets$edge)
    )
  })
}

# ── 6. Run sweeps ─────────────────────────────────────────────────────────────

high_odds_i3 <- preds_i3_odds  %>% filter(implied_prob_A < 0.29)
high_odds_wf <- preds_wf_odds  %>% filter(implied_prob_A < 0.29)
high_odds_ss <- preds_ss_odds  %>% filter(implied_prob_A < 0.29)

results_i3_high <- sweep_thresholds(high_odds_i3, "model_inter3",  "high_odds")
results_wf_high <- sweep_thresholds(high_odds_wf, "model_wform",   "high_odds")
results_ss_high <- sweep_thresholds(high_odds_ss, "SS_rule",        "high_odds")

# Also run on full (all implied probs) for context
results_i3_all  <- sweep_thresholds(preds_i3_odds, "model_inter3", "all_odds")
results_wf_all  <- sweep_thresholds(preds_wf_odds, "model_wform",  "all_odds")
results_ss_all  <- sweep_thresholds(preds_ss_odds, "SS_rule",       "all_odds")

results <- bind_rows(
  results_i3_high, results_wf_high, results_ss_high,
  results_i3_all,  results_wf_all,  results_ss_all
) %>%
  arrange(subset, threshold, model) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

# ── 7. Print ──────────────────────────────────────────────────────────────────

cat("\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("COMPARISON BACKTEST  |  2020-2024  |  Flat staking, 1 unit/bet\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("── HIGH-ODDS SUBSET (implied_prob_A < 0.29) ─────────────────────\n\n")
print(
  results %>%
    filter(subset == "high_odds") %>%
    select(model, threshold, n_bets, win_rate, total_pnl, roi, avg_odds, avg_edge),
  n = 30
)

cat("\n── FULL ODDS (all implied probs) ───────────────────────────────\n\n")
print(
  results %>%
    filter(subset == "all_odds") %>%
    select(model, threshold, n_bets, win_rate, total_pnl, roi, avg_odds, avg_edge),
  n = 30
)

# Dataset coverage summary
cat("\n── COVERAGE SUMMARY ─────────────────────────────────────────────\n")
tibble(
  approach   = c("model_inter3", "model_wform", "SS_rule"),
  total_matched = c(nrow(preds_i3_odds), nrow(preds_wf_odds), nrow(preds_ss_odds)),
  high_odds_n   = c(nrow(high_odds_i3), nrow(high_odds_wf), nrow(high_odds_ss)),
  high_odds_wr  = c(mean(high_odds_i3$won_bet), mean(high_odds_wf$won_bet), mean(high_odds_ss$won_bet))
) %>% print()

# ── 8. Save ───────────────────────────────────────────────────────────────────

out_path <- "C:/Users/User/OneDrive/Documents/tennis_model/backtest_results_comparison.csv"
write_csv(results, out_path)
cat("\nSaved to:", out_path, "\n")
