# round_feature_experiment.R
# Tests whether adding tournament_round (ordinal) improves model_inter3_elo.
#
# Decision rule:
#   UPDATE model if AUC improves > 0.005  OR  ROI improves > 5pp (at edge 0.15)
#                                         AND  z-score remains >= 3.5
#   DISCARD and document otherwise.
#
# If updated: overwrites model_inter3_elo.rds + updates live_predict.R + CLAUDE.md
# If discarded: documents null result in CLAUDE.md only

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

model_baseline <- readRDS("model_inter3_elo.rds")
elo_lookup     <- readRDS("elo_surface_lookup.rds")

# ════════════════════════════════════════════════════════════════════════════
# STEP 1 — Round mapping and join
# ════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════\n")
cat("ROUND FEATURE EXPERIMENT\n")
cat("Ordinal encoding: R128=1, R64=2, R32/RR=3, R16=4, QF=5, SF=6, F=7\n")
cat("BR and unknown rounds excluded.\n")
cat("══════════════════════════════════════════════════════\n\n")

ROUND_MAP <- c(R128 = 1L, R64 = 2L, R32 = 3L, RR = 3L,
               R16  = 4L, QF  = 5L, SF  = 6L, F  = 7L)

# Lookup tibble for dplyr join (named vector subscripting fails inside mutate)
round_lookup <- tibble(
  round     = names(ROUND_MAP),
  round_num = unname(ROUND_MAP)
)

# Check what round values actually appear
round_counts <- df_with_surface %>%
  count(round, sort = TRUE)
cat("Round value distribution in df_with_surface:\n")
print(round_counts, n = 20)
cat("\n")

add_round_num <- function(df) {
  # Build a match_id → round_num lookup directly from df_with_surface
  round_num_lookup <- df_with_surface %>%
    select(match_id, round) %>%
    left_join(round_lookup, by = "round") %>%
    select(match_id, round_num)

  # Drop any existing round/round_num columns before joining to avoid conflicts
  df %>%
    select(-any_of(c("round", "round_num"))) %>%
    left_join(round_num_lookup, by = "match_id")   # NA for BR / unmapped values
}

train_round <- add_round_num(train_inter)
test_round  <- add_round_num(test_inter)

cat(sprintf("Before round filter — train: %d  |  test: %d\n",
            nrow(train_round), nrow(test_round)))
cat(sprintf("Rows with NA round_num — train: %d  |  test: %d\n",
            sum(is.na(train_round$round_num)), sum(is.na(test_round$round_num))))
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# STEP 2 — Add Elo and clean (same filter logic as retrain_and_validate.R)
# ════════════════════════════════════════════════════════════════════════════

cat("Adding elo_diff_surface and applying NA filters...\n")

train_elo <- add_elo_feature(
  train_round %>% mutate(outcome = factor(outcome, levels = c("0", "1"))),
  elo_lookup
)
test_elo <- add_elo_feature(
  test_round %>% mutate(outcome = factor(outcome, levels = c("0", "1"))),
  elo_lookup
)

# Shared filter for both models — ensures same rows evaluated
# (round_num NA filter removes only BR rows, which are structurally uninformative)
train_clean <- train_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface),
         !is.na(round_num))

test_clean <- test_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface),
         !is.na(round_num))

cat(sprintf("Clean rows — train: %d  |  test: %d\n\n",
            nrow(train_clean), nrow(test_clean)))

# ════════════════════════════════════════════════════════════════════════════
# STEP 3 — Retrain both models on the SAME training rows
#           (baseline refit ensures fair AUC comparison on same test rows)
# ════════════════════════════════════════════════════════════════════════════

cat("Training baseline (4 features) and round variant (5 features)...\n")

model_base_refit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface,
      data = train_clean)

model_round_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface
                + round_num,
      data = train_clean)

cat("\n── Round model coefficients ──────────────────────────\n")
print(tidy(model_round_fit$fit))
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# STEP 4 — AUC / Brier on test set (same rows for both)
# ════════════════════════════════════════════════════════════════════════════

eval_model <- function(model, test_df, label) {
  p <- predict(model, test_df, type = "prob")$.pred_1
  y <- as.integer(as.character(test_df$outcome))
  acc   <- mean((p >= 0.5) == (y == 1))
  brier <- mean((p - y)^2)
  r     <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  auc   <- (sum(r[y == 1]) - n1*(n1+1)/2) / (n1 * n0)
  tibble(model     = label,
         n         = nrow(test_df),
         accuracy  = round(acc,   4),
         brier     = round(brier, 4),
         auc       = round(auc,   4))
}

cat("── Test set metrics (2020-2024, same rows for both models) ──────\n")
metrics <- bind_rows(
  eval_model(model_base_refit, test_clean, "baseline (4 features)"),
  eval_model(model_round_fit,  test_clean, "round variant (5 features)")
)
print(metrics, n = 5)
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# STEP 5 — Odds join helper (reused from retrain_and_validate.R)
# ════════════════════════════════════════════════════════════════════════════

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
  p  <- predict(model, feat_df, type = "prob")$.pred_1
  df <- feat_df %>% mutate(prob = p)

  prep <- df %>%
    mutate(
      playerA_abbrev = map_chr(playerA, abbreviate_name),
      playerB_abbrev = map_chr(playerB, abbreviate_name),
      surface_clean  = as.character(surface),
      td_str         = as.character(tourney_date),
      date_key       = paste0(substr(td_str, 1, 4), "-", substr(td_str, 5, 6))
    )

  a_wins <- prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev" = "winner_abbrev",
                      "playerB_abbrev" = "loser_abbrev",
                      "surface_clean"  = "surface_odds",
                      "date_key"       = "date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365W, implied_prob_A = norm_prob_W)

  a_loses <- prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev" = "loser_abbrev",
                      "playerB_abbrev" = "winner_abbrev",
                      "surface_clean"  = "surface_odds",
                      "date_key"       = "date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365L, implied_prob_A = norm_prob_L)

  bind_rows(a_wins, a_loses) %>%
    mutate(
      outcome_num = as.integer(as.character(outcome)),
      won_bet     = outcome_num == 1,
      edge        = prob - implied_prob_A,
      model_label = label
    ) %>%
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

cat("Building odds join...\n")
preds_base  <- join_and_dedup(test_clean, model_base_refit, "baseline")
preds_round <- join_and_dedup(test_clean, model_round_fit,  "round_variant")

ho_base  <- preds_base  %>% filter(implied_prob_A < 0.29)
ho_round <- preds_round %>% filter(implied_prob_A < 0.29)

cat(sprintf("High-odds rows matched — baseline: %d  |  round: %d\n\n",
            nrow(ho_base), nrow(ho_round)))

# ════════════════════════════════════════════════════════════════════════════
# STEP 6 — Backtest sweep
# ════════════════════════════════════════════════════════════════════════════

sweep <- function(df, label, thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25)) {
  map_dfr(thresholds, function(thr) {
    bets <- df %>% filter(edge >= thr)
    n    <- nrow(bets)
    if (n == 0) return(tibble(model     = label, threshold = thr, n_bets    = 0L,
                              win_rate  = NA_real_, roi = NA_real_,
                              avg_odds  = NA_real_, avg_edge = NA_real_))
    tibble(
      model     = label,
      threshold = thr,
      n_bets    = n,
      win_rate  = mean(bets$won_bet),
      roi       = sum(if_else(bets$won_bet, bets$odds_A - 1, -1)) / n,
      avg_odds  = mean(bets$odds_A),
      avg_edge  = mean(bets$edge)
    )
  })
}

backtest <- bind_rows(
  sweep(ho_base,  "baseline"),
  sweep(ho_round, "round_variant")
) %>%
  arrange(threshold, model) %>%
  mutate(across(where(is.numeric) & !c(threshold, n_bets), ~ round(., 4)))

cat("══════════════════════════════════════════════════════════════════\n")
cat("BACKTEST  |  HIGH-ODDS (implied < 0.29)  |  2020-2024  |  flat staking\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
print(backtest %>% arrange(threshold, model), n = 30)

# ════════════════════════════════════════════════════════════════════════════
# STEP 7 — Significance tests (primary threshold = 0.15)
# ════════════════════════════════════════════════════════════════════════════

run_significance <- function(bets_df, threshold, label) {
  bets <- bets_df %>% filter(edge >= threshold)
  n    <- nrow(bets)
  if (n == 0) return(NULL)

  wins  <- sum(bets$won_bet)
  wr    <- wins / n
  ps    <- bets$implied_prob_A
  p_avg <- mean(ps)

  exp_w <- sum(ps)
  sd_w  <- sqrt(sum(ps * (1 - ps)))
  z     <- (wins - exp_w) / sd_w
  p_pb  <- pnorm(z, lower.tail = FALSE)

  roi   <- sum(if_else(bets$won_bet, bets$odds_A - 1, -1)) / n

  tibble(
    model        = label,
    threshold    = threshold,
    n_bets       = n,
    win_rate     = round(wr,    4),
    avg_implied  = round(p_avg, 4),
    true_edge    = round(wr - p_avg, 4),
    roi          = round(roi,   4),
    z_score      = round(z,     3),
    p_value      = round(p_pb,  5),
    significant  = p_pb < 0.05
  )
}

sig_results <- bind_rows(
  map_dfr(c(0.10, 0.15, 0.20), ~ run_significance(ho_base,  .x, "baseline")),
  map_dfr(c(0.10, 0.15, 0.20), ~ run_significance(ho_round, .x, "round_variant"))
) %>% arrange(threshold, model)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("SIGNIFICANCE TESTS  |  Poisson-binomial, one-sided\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
print(sig_results, n = 20)

# ════════════════════════════════════════════════════════════════════════════
# STEP 8 — Decision gate
# ════════════════════════════════════════════════════════════════════════════

AUC_THRESHOLD  <- 0.005   # minimum AUC improvement to update
ROI_THRESHOLD  <- 0.05    # minimum ROI improvement (absolute, e.g. 5pp)
Z_MIN          <- 3.5     # round model must maintain this z-score

base_auc  <- metrics %>% filter(grepl("baseline", model)) %>% pull(auc)
round_auc <- metrics %>% filter(grepl("round",    model)) %>% pull(auc)
auc_delta <- round_auc - base_auc

base_roi  <- sig_results %>% filter(model == "baseline",      threshold == 0.15) %>% pull(roi)
round_roi <- sig_results %>% filter(model == "round_variant", threshold == 0.15) %>% pull(roi)
round_z   <- sig_results %>% filter(model == "round_variant", threshold == 0.15) %>% pull(z_score)
roi_delta <- round_roi - base_roi

auc_passes  <- !is.na(auc_delta)  && auc_delta  > AUC_THRESHOLD
roi_passes  <- !is.na(roi_delta)  && roi_delta  > ROI_THRESHOLD && !is.na(round_z) && round_z >= Z_MIN

should_update <- auc_passes || roi_passes

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("DECISION\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  AUC   baseline=%.4f  round=%.4f  delta=%+.4f  threshold=%+.3f  → %s\n",
            base_auc, round_auc, auc_delta, AUC_THRESHOLD,
            if (auc_passes) "PASS" else "FAIL"))
cat(sprintf("  ROI   baseline=%+.1f%%  round=%+.1f%%  delta=%+.1f%%  threshold=+%.0f%%  z=%.2f  → %s\n",
            base_roi * 100, round_roi * 100, roi_delta * 100, ROI_THRESHOLD * 100,
            ifelse(is.na(round_z), 0, round_z),
            if (roi_passes) "PASS" else "FAIL"))
cat("\n")

if (should_update) {
  cat("*** DECISION: UPDATE model_inter3_elo with round_num feature ***\n\n")
} else {
  cat("*** DECISION: DISCARD — round_num does not improve model sufficiently ***\n\n")
}

# ════════════════════════════════════════════════════════════════════════════
# STEP 9 — Conditional update
# ════════════════════════════════════════════════════════════════════════════

if (should_update) {

  cat("Saving model_inter3_elo.rds with round_num feature...\n")
  model_inter3_elo <- model_round_fit
  saveRDS(model_inter3_elo, "model_inter3_elo.rds")
  cat("Saved.\n\n")

  cat("NOTE: live_predict.R must be updated — see instructions below.\n")
  cat("  Add to live_predict.R:\n")
  cat("    ROUND_MAP <- c(R128=1L, R64=2L, R32=3L, RR=3L, R16=4L, QF=5L, SF=6L, F=7L)\n")
  cat("    New param in predict_match(): tournament_round = 'R32'\n")
  cat("    round_num <- as.integer(ROUND_MAP[tournament_round])\n")
  cat("    Add round_num to match_input data.frame()\n\n")

  cat("CLAUDE.md update needed:\n")
  cat(sprintf("  model_inter3_elo formula now includes round_num\n"))
  cat(sprintf("  New AUC: %.4f  |  Brier: %.4f\n",
              round_auc,
              metrics %>% filter(grepl("round", model)) %>% pull(brier)))
  cat(sprintf("  New ROI at edge>0.15: %+.1f%%  z=%.2f\n",
              round_roi * 100, round_z))

} else {

  cat("No changes to model_inter3_elo.rds.\n\n")
  cat("CLAUDE.md null result to document:\n")
  cat(sprintf("  round_num coefficient: %.4f (p = ...)\n",
              coef(model_round_fit$fit)["round_num"]))
  cat(sprintf("  AUC delta:  %+.4f  (threshold: +0.005) — %s\n",
              auc_delta, if (auc_passes) "PASS" else "below threshold"))
  cat(sprintf("  ROI delta:  %+.1f%%  (threshold: +5pp)  — %s\n",
              roi_delta * 100, if (roi_passes) "PASS" else "below threshold"))
  cat(sprintf("  z-score (round model, edge>0.15): %.2f\n",
              ifelse(is.na(round_z), 0, round_z)))
  cat("  Conclusion: round is structurally determined by rank/Elo/games_dom;\n")
  cat("              it does not add independent predictive signal.\n")

}

cat("\nDone.\n")
