# elo_init_experiment.R
# Tests rank-based Elo initialisation vs flat 1500 baseline.
#
# New init: 1500 + 100 * (1 - log(rank) / log(500)), capped to [1400, 1700]
#   rank=1   → 1600  |  rank=10  → ~1563  |  rank=100 → ~1526
#   rank=500 → 1500  |  rank>500 → <1500, floored at 1400
#
# Decision rule: keep new artefacts only if BOTH new_auc >= 0.800 AND new_roi >= 0.625.
# Reverts elo_surface_lookup.rds and model_inter3_elo.rds otherwise.
# Documents the result in CLAUDE.md Null Result Experiments table either way.
#
# Usage: Rscript elo_init_experiment.R

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

# ── Baselines to beat ─────────────────────────────────────────────────────────
BASELINE_AUC <- 0.800
BASELINE_ROI <- 0.625

# ── Step 0: Backup current artefacts ─────────────────────────────────────────
cat("── Backing up current artefacts ────────────────────────────────────────\n")
file.copy("elo_surface_lookup.rds", "elo_surface_lookup_backup.rds", overwrite = TRUE)
file.copy("model_inter3_elo.rds",   "model_inter3_elo_backup.rds",   overwrite = TRUE)
cat("Backups created.\n\n")

# Load baseline model for side-by-side comparison
model_saved    <- readRDS("model_inter3_elo_backup.rds")
elo_lookup_old <- readRDS("elo_surface_lookup_backup.rds")

# ── Step 1: Build rank_init_df ────────────────────────────────────────────────
# For each (player, surface), take the rank from their chronologically first match.
cat("── Building rank_init_df ────────────────────────────────────────────────\n")

sorted_matches <- df_with_surface %>% arrange(tourney_date, match_num)

rank_init_df <- bind_rows(
  sorted_matches %>% transmute(player = winner_name, surface, rank = winner_rank),
  sorted_matches %>% transmute(player = loser_name,  surface, rank = loser_rank)
) %>%
  filter(!is.na(rank)) %>%
  group_by(player, surface) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("rank_init_df: %d rows  |  unique players: %d  |  surfaces: %s\n\n",
            nrow(rank_init_df),
            n_distinct(rank_init_df$player),
            paste(sort(unique(rank_init_df$surface)), collapse = ", ")))

# Spot-check the formula
cat("── Formula check (sample ranks) ────────────────────────────────────────\n")
sample_ranks <- c(1, 10, 50, 100, 250, 500, 750)
cat(sprintf("%-8s  %s\n", "rank", "init_elo"))
for (r in sample_ranks) {
  cat(sprintf("%-8d  %.1f\n", r, max(1400, min(1700, 1500 + 100 * (1 - log(r) / log(500))))))
}
cat("\n")

# ── Step 2: Recompute elo_surface_lookup with rank-based init ─────────────────
cat("── Recomputing elo_surface_lookup (rank-based init) ────────────────────\n")
elo_lookup_new <- compute_surface_elo(df_with_surface, K = 32, rank_init_df = rank_init_df)
cat(sprintf("New lookup: %d rows\n\n", nrow(elo_lookup_new)))

# ── Step 3: Confirm train/test split ─────────────────────────────────────────
cat("── Train / test split ───────────────────────────────────────────────────\n")
train_years <- range(as.integer(substr(as.character(train_inter$tourney_date), 1, 4)))
test_years  <- range(as.integer(substr(as.character(test_inter$tourney_date),  1, 4)))
cat(sprintf("train_inter: %d rows  |  years %d–%d\n", nrow(train_inter), train_years[1], train_years[2]))
cat(sprintf("test_inter:  %d rows  |  years %d–%d\n\n", nrow(test_inter),  test_years[1],  test_years[2]))

# ── Step 4: Add Elo features using new lookup ─────────────────────────────────
to_factor <- function(df) df %>% mutate(outcome = factor(outcome, levels = c("0","1")))

train_elo_new <- add_elo_feature(to_factor(train_inter), elo_lookup_new)
test_elo_new  <- add_elo_feature(to_factor(test_inter),  elo_lookup_new)
train_elo_old <- add_elo_feature(to_factor(train_inter), elo_lookup_old)
test_elo_old  <- add_elo_feature(to_factor(test_inter),  elo_lookup_old)

na_cols <- c("log_rank_ratio", "games_dom_diff", "games_dom_x_underdog", "elo_diff_surface")
clean <- function(df) df %>% filter(if_all(all_of(na_cols), ~ !is.na(.)))

train_new <- clean(train_elo_new);  test_new <- clean(test_elo_new)
train_old <- clean(train_elo_old);  test_old <- clean(test_elo_old)

cat(sprintf("train rows — new: %d  |  old: %d\n", nrow(train_new), nrow(train_old)))
cat(sprintf("test  rows — new: %d  |  old: %d\n\n", nrow(test_new),  nrow(test_old)))

# ── Step 5: Retrain models ────────────────────────────────────────────────────
cat("── Training models ──────────────────────────────────────────────────────\n")
formula_inter3_elo <- outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface

model_new <- logistic_reg() %>% set_engine("glm") %>% fit(formula_inter3_elo, data = train_new)
model_old <- logistic_reg() %>% set_engine("glm") %>% fit(formula_inter3_elo, data = train_old)

cat("\n── Coefficients: new (rank init) vs old (flat 1500) ────────────────────\n")
coef_new <- coef(model_new$fit)
coef_old <- coef(model_old$fit)
print(data.frame(
  term    = names(coef_new),
  new     = round(coef_new, 6),
  old     = round(coef_old, 6),
  delta   = round(coef_new - coef_old, 8)
), row.names = FALSE)

# ── Step 6: AUC / Accuracy / Brier ───────────────────────────────────────────
eval_model <- function(model, test_df, label) {
  p  <- predict(model, test_df, type = "prob")$.pred_1
  y  <- as.integer(as.character(test_df$outcome))
  r  <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  tibble(
    model    = label,
    n        = nrow(test_df),
    accuracy = round(mean((p >= 0.5) == (y == 1)), 4),
    auc      = round((sum(r[y == 1]) - n1*(n1+1)/2) / (n1*n0), 4),
    brier    = round(mean((p - y)^2), 4)
  )
}

cat("\n── Predictive accuracy (2020–2024 test set) ─────────────────────────────\n")
metrics <- bind_rows(
  eval_model(model_new, test_new, "new (rank init)"),
  eval_model(model_old, test_old, "old (flat 1500)")
)
print(metrics, n = 5)
new_auc <- metrics$auc[metrics$model == "new (rank init)"]

# ── Step 7: Backtest ──────────────────────────────────────────────────────────
cat("\n── Building backtest odds join ───────────────────────────────────────────\n")

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

join_and_dedup <- function(feat_df, model, label) {
  p <- predict(model, feat_df, type = "prob")$.pred_1
  df <- feat_df %>% mutate(prob = p) %>%
    mutate(
      playerA_abbrev = map_chr(playerA, abbreviate_name),
      playerB_abbrev = map_chr(playerB, abbreviate_name),
      surface_clean  = as.character(surface),
      td_str         = as.character(tourney_date),
      date_key       = paste0(substr(td_str,1,4), "-", substr(td_str,5,6))
    )
  a_wins <- df %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="winner_abbrev","playerB_abbrev"="loser_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365W, implied_prob_A = norm_prob_W)
  a_loses <- df %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="loser_abbrev","playerB_abbrev"="winner_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365L, implied_prob_A = norm_prob_L)
  bind_rows(a_wins, a_loses) %>%
    mutate(outcome_num = as.integer(as.character(outcome)),
           won_bet     = outcome_num == 1,
           edge        = prob - implied_prob_A,
           model_label = label) %>%
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

preds_new <- join_and_dedup(test_new, model_new, "new (rank init)")
preds_old <- join_and_dedup(test_old, model_old, "old (flat 1500)")

ho_new <- preds_new %>% filter(implied_prob_A < 0.29)
ho_old <- preds_old %>% filter(implied_prob_A < 0.29)

sweep <- function(df, label, thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25)) {
  map_dfr(thresholds, function(thr) {
    bets <- df %>% filter(edge >= thr)
    n    <- nrow(bets)
    if (n == 0) return(tibble(model=label, threshold=thr, n_bets=0L,
                              win_rate=NA_real_, roi=NA_real_, avg_odds=NA_real_))
    tibble(
      model     = label,
      threshold = thr,
      n_bets    = n,
      win_rate  = round(mean(bets$won_bet), 4),
      roi       = round(sum(if_else(bets$won_bet, bets$odds_A - 1, -1)) / n, 4),
      avg_odds  = round(mean(bets$odds_A), 2)
    )
  })
}

backtest <- bind_rows(sweep(ho_new, "new"), sweep(ho_old, "old")) %>%
  arrange(threshold, model)

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("BACKTEST  |  implied < 0.29  |  2020–2024  |  flat staking\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")
print(backtest, n = 30)

new_roi  <- backtest %>% filter(model == "new", threshold == 0.15) %>% pull(roi)
new_bets <- backtest %>% filter(model == "new", threshold == 0.15) %>% pull(n_bets)

# ── Step 8: Significance test at 0.15 ────────────────────────────────────────
run_sig <- function(ho_df, threshold, label) {
  bets  <- ho_df %>% filter(edge >= threshold)
  n     <- nrow(bets)
  if (n == 0) return(NULL)
  wins  <- sum(bets$won_bet)
  ps    <- bets$implied_prob_A
  z     <- (wins - sum(ps)) / sqrt(sum(ps * (1 - ps)))
  p_pb  <- pnorm(z, lower.tail = FALSE)
  tibble(model=label, n_bets=n, wins=wins,
         win_rate=round(wins/n,4), avg_implied=round(mean(ps),4),
         z=round(z,3), p=round(p_pb,5))
}

cat("\n── Significance at edge ≥ 0.15 ──────────────────────────────────────────\n")
print(bind_rows(
  run_sig(ho_new, 0.15, "new (rank init)"),
  run_sig(ho_old, 0.15, "old (flat 1500)")
))

# ── Step 9: Decision ──────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════════\n")
cat(sprintf("DECISION  |  new AUC: %.4f (baseline %.3f)  |  new ROI: %+.1f%% (baseline %+.1f%%)\n",
            new_auc, BASELINE_AUC, 100*new_roi, 100*BASELINE_ROI))

keep <- !is.na(new_auc) && !is.na(new_roi) && new_auc >= BASELINE_AUC && new_roi >= BASELINE_ROI

if (keep) {
  saveRDS(elo_lookup_new, "elo_surface_lookup.rds")
  saveRDS(model_new,      "model_inter3_elo.rds")
  file.remove("elo_surface_lookup_backup.rds")
  file.remove("model_inter3_elo_backup.rds")
  cat("[KEEP] New model and lookup saved. Backups removed.\n")
} else {
  file.copy("elo_surface_lookup_backup.rds", "elo_surface_lookup.rds", overwrite = TRUE)
  file.copy("model_inter3_elo_backup.rds",   "model_inter3_elo.rds",   overwrite = TRUE)
  file.remove("elo_surface_lookup_backup.rds")
  file.remove("model_inter3_elo_backup.rds")
  cat("[REVERT] AUC or ROI did not improve. Baseline restored.\n")
}

cat("══════════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("Summary for CLAUDE.md:\n"))
cat(sprintf("  AUC:  new=%.4f  baseline=%.4f  delta=%+.4f\n",
            new_auc, BASELINE_AUC, new_auc - BASELINE_AUC))
cat(sprintf("  ROI:  new=%+.1f%%  baseline=%+.1f%%  delta=%+.1f%%\n",
            100*new_roi, 100*BASELINE_ROI, 100*(new_roi - BASELINE_ROI)))
cat(sprintf("  Bets: new=%d  baseline=94\n", new_bets))
cat(sprintf("  Outcome: %s\n\n", if (keep) "KEPT — update Design Decisions" else "REVERTED — add to Null Results"))

cat("Done.\n")
