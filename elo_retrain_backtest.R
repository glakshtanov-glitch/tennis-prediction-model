# elo_retrain_backtest.R
# 1. Compute surface Elo from df_with_surface
# 2. Add elo_diff_surface to train_inter / test_inter
# 3. Retrain model_inter3 with new feature
# 4. Rerun the comparison backtest (model_inter3_elo vs original model_inter3)
# 5. Save elo_lookup as elo_surface_lookup.rds and results to backtest_results_elo.csv

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

# ════════════════════════════════════════════════════════════════
# STEP 1 — Compute surface-specific Elo
# ════════════════════════════════════════════════════════════════

cat("Computing surface Elo on", nrow(df_with_surface), "matches...\n")
t0 <- proc.time()

elo_lookup <- compute_surface_elo(df_with_surface, K = 32, init_elo = 1500)

cat(sprintf("Done in %.1f sec. Rows: %d\n", (proc.time() - t0)["elapsed"], nrow(elo_lookup)))

# Sanity check: Elo distribution
cat("\nElo pre-match distribution (winners):\n")
print(summary(elo_lookup$elo_winner_pre))
cat("Elo pre-match distribution (losers):\n")
print(summary(elo_lookup$elo_loser_pre))

# Save lookup for reuse
saveRDS(elo_lookup, "elo_surface_lookup.rds")
cat("Saved elo_surface_lookup.rds\n\n")

# ════════════════════════════════════════════════════════════════
# STEP 2 — Add elo_diff_surface to train_inter / test_inter
# ════════════════════════════════════════════════════════════════

cat("Adding elo_diff_surface to train_inter...\n")
train_inter_elo <- add_elo_feature(
  train_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
)

cat("Adding elo_diff_surface to test_inter...\n")
test_inter_elo <- add_elo_feature(
  test_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
)

# Check coverage
cat("\ntrain_inter_elo NA elo_diff_surface:", sum(is.na(train_inter_elo$elo_diff_surface)), "/", nrow(train_inter_elo), "\n")
cat("test_inter_elo  NA elo_diff_surface:", sum(is.na(test_inter_elo$elo_diff_surface)),  "/", nrow(test_inter_elo),  "\n")

cat("\nElo diff summary (train):\n")
print(summary(train_inter_elo$elo_diff_surface))

# ════════════════════════════════════════════════════════════════
# STEP 3 — Retrain model_inter3 with elo_diff_surface
# ════════════════════════════════════════════════════════════════

cat("\nRetraining model_inter3 + elo_diff_surface...\n")

# Original: outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog
# New:      outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface

model_inter3_elo <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface,
      data = train_inter_elo %>%
        filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
               !is.na(games_dom_x_underdog), !is.na(elo_diff_surface)))

cat("\n--- model_inter3_elo coefficients ---\n")
print(coef(model_inter3_elo$fit))

cat("\n--- original model_inter3 coefficients (for comparison) ---\n")
print(coef(model_inter3$fit))

# ════════════════════════════════════════════════════════════════
# STEP 4 — Evaluate on test set
# ════════════════════════════════════════════════════════════════

test_clean_elo <- test_inter_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))

test_clean_orig <- test_inter_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog))

# Accuracy + Brier
eval_model <- function(model, test_df, label) {
  p <- predict(model, test_df, type = "prob")$.pred_1
  y <- as.integer(as.character(test_df$outcome))
  acc   <- mean((p >= 0.5) == (y == 1))
  brier <- mean((p - y)^2)
  auc   <- suppressMessages({
    r <- rank(p)
    n1 <- sum(y == 1); n0 <- sum(y == 0)
    (sum(r[y == 1]) - n1*(n1+1)/2) / (n1 * n0)
  })
  cat(sprintf("%-35s  Acc=%.3f  Brier=%.4f  AUC=%.4f\n", label, acc, brier, auc))
  tibble(model = label, accuracy = acc, brier = brier, auc = auc)
}

cat("\n--- Model accuracy comparison (test: 2020-2024) ---\n")
metrics <- bind_rows(
  eval_model(model_inter3,     test_clean_orig, "model_inter3 (original)"),
  eval_model(model_inter3_elo, test_clean_elo,  "model_inter3 + elo_diff_surface")
)

# ════════════════════════════════════════════════════════════════
# STEP 5 — Backtest: model_inter3_elo vs model_inter3 (original)
# ════════════════════════════════════════════════════════════════

# Reuse the odds join logic from compare_backtest.R
cat("\n\nBuilding odds join...\n")

odds_prep <- odds_all %>%
  rename(surface_odds = Surface) %>%
  filter(!is.na(B365W), !is.na(B365L)) %>%
  mutate(
    date_parts  = str_split(Date, "/"),
    odds_month  = map_chr(date_parts, ~ sprintf("%02d", as.integer(.x[1]))),
    odds_year   = map_chr(date_parts, ~ .x[3]),
    date_key    = paste0(odds_year, "-", odds_month),
    overround   = 1/B365W + 1/B365L,
    norm_prob_W = (1/B365W) / overround,
    norm_prob_L = (1/B365L) / overround
  ) %>%
  select(winner_abbrev = Winner, loser_abbrev = Loser,
         surface_odds, date_key, B365W, B365L, norm_prob_W, norm_prob_L)

join_and_dedup <- function(feat_df) {
  feat_prep <- feat_df %>%
    mutate(
      playerA_abbrev = map_chr(playerA, abbreviate_name),
      playerB_abbrev = map_chr(playerB, abbreviate_name),
      surface_clean  = as.character(surface),
      td_str         = as.character(tourney_date),
      date_key       = paste0(substr(td_str, 1, 4), "-", substr(td_str, 5, 6))
    )

  join_a_wins <- feat_prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev" = "winner_abbrev",
                      "playerB_abbrev" = "loser_abbrev",
                      "surface_clean"  = "surface_odds",
                      "date_key"       = "date_key"),
               relationship = "many-to-many") %>%
    mutate(playerA_is_winner = TRUE,  odds_A = B365W, implied_prob_A = norm_prob_W)

  join_a_loses <- feat_prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev" = "loser_abbrev",
                      "playerB_abbrev" = "winner_abbrev",
                      "surface_clean"  = "surface_odds",
                      "date_key"       = "date_key"),
               relationship = "many-to-many") %>%
    mutate(playerA_is_winner = FALSE, odds_A = B365L, implied_prob_A = norm_prob_L)

  bind_rows(join_a_wins, join_a_loses) %>%
    mutate(outcome_num = as.integer(as.character(outcome))) %>%
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

sweep_thresholds <- function(df, label, thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25)) {
  map_dfr(thresholds, function(thr) {
    bets <- df %>% filter(edge >= thr)
    if (nrow(bets) == 0) return(tibble(model=label, threshold=thr, n_bets=0L,
                                       win_rate=NA, total_pnl=NA, roi=NA,
                                       avg_odds=NA, avg_edge=NA))
    tibble(model     = label,
           threshold = thr,
           n_bets    = nrow(bets),
           win_rate  = mean(bets$won_bet),
           total_pnl = sum(if_else(bets$won_bet, bets$odds_A - 1, -1)),
           roi       = total_pnl / n_bets,
           avg_odds  = mean(bets$odds_A),
           avg_edge  = mean(bets$edge))
  })
}

# ── Original model_inter3 predictions ────────────────────────────────────────
cat("Generating model_inter3 (original) predictions...\n")
preds_orig <- predict(model_inter3, test_inter_elo %>%
                        filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
                               !is.na(games_dom_x_underdog)),
                      type = "prob") %>%
  rename(prob = .pred_1) %>%
  bind_cols(test_inter_elo %>%
              filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
                     !is.na(games_dom_x_underdog)))

preds_orig_odds <- join_and_dedup(preds_orig) %>%
  mutate(edge = prob - implied_prob_A, won_bet = outcome_num == 1)

# ── New model_inter3_elo predictions ─────────────────────────────────────────
cat("Generating model_inter3_elo predictions...\n")
preds_elo <- predict(model_inter3_elo, test_clean_elo, type = "prob") %>%
  rename(prob = .pred_1) %>%
  bind_cols(test_clean_elo)

preds_elo_odds <- join_and_dedup(preds_elo) %>%
  mutate(edge = prob - implied_prob_A, won_bet = outcome_num == 1)

# ── High-odds subsets ─────────────────────────────────────────────────────────
ho_orig <- preds_orig_odds %>% filter(implied_prob_A < 0.29)
ho_elo  <- preds_elo_odds  %>% filter(implied_prob_A < 0.29)

cat("Original model_inter3 high-odds rows:", nrow(ho_orig), "\n")
cat("model_inter3 + elo    high-odds rows:", nrow(ho_elo),  "\n")

# ── Run sweeps ────────────────────────────────────────────────────────────────
results_all <- bind_rows(
  sweep_thresholds(ho_orig,         "model_inter3 (original)", ) %>% mutate(subset = "high_odds"),
  sweep_thresholds(ho_elo,          "model_inter3 + elo_surf") %>% mutate(subset = "high_odds"),
  sweep_thresholds(preds_orig_odds, "model_inter3 (original)") %>% mutate(subset = "all_odds"),
  sweep_thresholds(preds_elo_odds,  "model_inter3 + elo_surf") %>% mutate(subset = "all_odds")
) %>%
  arrange(subset, threshold, model) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

# ════════════════════════════════════════════════════════════════
# STEP 6 — Print results
# ════════════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("ELO BACKTEST  |  2020-2024  |  Flat staking, 1 unit/bet\n")
cat("═══════════════════════════════════════════════════════════════════\n")

cat("\n── Prediction accuracy ─────────────────────────────────────────────\n")
print(metrics)

cat("\n── HIGH-ODDS (implied_prob_A < 0.29) ───────────────────────────────\n\n")
print(
  results_all %>% filter(subset == "high_odds") %>%
    select(model, threshold, n_bets, win_rate, total_pnl, roi, avg_odds, avg_edge),
  n = 20
)

cat("\n── FULL ODDS ────────────────────────────────────────────────────────\n\n")
print(
  results_all %>% filter(subset == "all_odds") %>%
    select(model, threshold, n_bets, win_rate, total_pnl, roi, avg_odds, avg_edge),
  n = 20
)

cat("\n── Elo distribution at test period (2020-2024) ──────────────────────\n")
test_inter_elo %>%
  filter(!is.na(elo_diff_surface)) %>%
  mutate(year = as.integer(substr(as.character(tourney_date), 1, 4))) %>%
  filter(year >= 2020) %>%
  summarise(
    mean_elo_diff = mean(elo_diff_surface),
    sd_elo_diff   = sd(elo_diff_surface),
    pct_correct   = mean(elo_diff_surface > 0 & as.integer(as.character(outcome)) == 1 |
                           elo_diff_surface < 0 & as.integer(as.character(outcome)) == 0)
  ) %>% print()

# ════════════════════════════════════════════════════════════════
# STEP 7 — Save
# ════════════════════════════════════════════════════════════════

out_path <- "C:/Users/User/OneDrive/Documents/tennis_model/backtest_results_elo.csv"
write_csv(results_all, out_path)
cat("\nSaved to:", out_path, "\n")

saveRDS(model_inter3_elo,
        "C:/Users/User/OneDrive/Documents/tennis_model/model_inter3_elo.rds")
cat("Saved model_inter3_elo.rds\n")
