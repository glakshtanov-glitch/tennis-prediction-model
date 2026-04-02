# xgb_backtest.R
# Train XGBoost with same features as model_wform, then backtest both models
# on the high-odds subset (implied_prob_A < 0.29) sweeping edge thresholds.
# Saves results to backtest_results.csv.

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")

library(tidymodels)
library(xgboost)

# ── 0. Prep model dataset ──────────────────────────────────────────────────────

model_df <- df_features6 %>%
  mutate(
    year           = as.integer(substr(as.character(tourney_date), 1, 4)),
    log_rank_ratio = log(rankB / rankA),
    wform_diff     = wformA - wformB,
    outcome        = factor(outcome, levels = c("0", "1"))
  ) %>%
  filter(!is.na(log_rank_ratio), !is.na(wform_diff), !is.na(surface))

train_df <- model_df %>% filter(year <= 2022)
test_df  <- model_df %>% filter(year >= 2023)

cat("Train rows:", nrow(train_df), "| Test rows:", nrow(test_df), "\n")

# ── 1. Train XGBoost ───────────────────────────────────────────────────────────

cat("\nTraining XGBoost...\n")

rec_xgb <- recipe(outcome ~ log_rank_ratio + wform_diff + surface,
                  data = train_df) %>%
  step_dummy(surface, one_hot = FALSE) %>%
  step_impute_median(all_numeric_predictors())

spec_xgb <- boost_tree(
  trees        = 500,
  learn_rate   = 0.05,
  tree_depth   = 4,
  min_n        = 10,
  loss_reduction = 0,
  sample_size  = 0.8,
  mtry         = 3
) %>%
  set_engine("xgboost", nthread = 1) %>%
  set_mode("classification")

wf_xgb <- workflow() %>%
  add_recipe(rec_xgb) %>%
  add_model(spec_xgb)

model_xgb_new <- fit(wf_xgb, data = train_df)
cat("XGBoost training complete.\n")

# ── 2. Generate predictions on test set ───────────────────────────────────────

cat("\nGenerating predictions...\n")

preds_log <- predict(model_wform, test_df, type = "prob") %>%
  rename(prob_logistic = .pred_1) %>%
  select(prob_logistic) %>%
  bind_cols(test_df)

preds_xgb_out <- predict(model_xgb_new, test_df, type = "prob") %>%
  rename(prob_xgb = .pred_1) %>%
  select(prob_xgb) %>%
  bind_cols(test_df)

preds_combined <- preds_log %>%
  left_join(preds_xgb_out %>% select(playerA, playerB, tourney_date, match_id, prob_xgb),
            by = c("playerA", "playerB", "tourney_date", "match_id"))

cat("Logistic predictions:", nrow(preds_log), "\n")
cat("XGBoost  predictions:", nrow(preds_xgb_out), "\n")

# ── 3. Join with odds_all ──────────────────────────────────────────────────────

cat("\nJoining with odds...\n")

# odds_all already has abbreviated names ("Surname F.") — do not re-abbreviate
odds_prep <- odds_all %>%
  rename(
    winner_abbrev = Winner,
    loser_abbrev  = Loser,
    surface_odds  = Surface
  ) %>%
  mutate(
    odds_year       = as.integer(substr(Date, nchar(Date) - 3, nchar(Date))),
    implied_prob_W  = 1 / B365W,
    implied_prob_L  = 1 / B365L,
    overround       = implied_prob_W + implied_prob_L,
    norm_prob_W     = implied_prob_W / overround,
    norm_prob_L     = implied_prob_L / overround
  ) %>%
  filter(!is.na(B365W), !is.na(B365L))

# Abbreviate playerA / playerB in predictions
preds_abbrev <- preds_combined %>%
  mutate(
    playerA_abbrev = map_chr(playerA, abbreviate_name),
    playerB_abbrev = map_chr(playerB, abbreviate_name),
    pred_year      = year,
    surface_clean  = as.character(surface)
  )

# Join where playerA = winner in odds (outcome will be 1 for these)
join_A_wins <- preds_abbrev %>%
  inner_join(
    odds_prep,
    by = c("playerA_abbrev" = "winner_abbrev",
           "playerB_abbrev" = "loser_abbrev",
           "surface_clean"  = "surface_odds",
           "pred_year"      = "odds_year")
  ) %>%
  mutate(
    playerA_is_winner = TRUE,
    odds_A            = B365W,
    implied_prob_A    = norm_prob_W
  )

# Join where playerA = loser in odds
join_A_loses <- preds_abbrev %>%
  inner_join(
    odds_prep,
    by = c("playerA_abbrev" = "loser_abbrev",
           "playerB_abbrev" = "winner_abbrev",
           "surface_clean"  = "surface_odds",
           "pred_year"      = "odds_year")
  ) %>%
  mutate(
    playerA_is_winner = FALSE,
    odds_A            = B365L,
    implied_prob_A    = norm_prob_L
  )

preds_with_odds <- bind_rows(join_A_wins, join_A_loses) %>%
  mutate(outcome_num = as.integer(as.character(outcome))) %>%
  # Remove any duplicate matches (same match_id joined twice)
  distinct(match_id, playerA_is_winner, .keep_all = TRUE)

cat("Predictions with odds:", nrow(preds_with_odds), "\n")

# ── 4. High-odds subset ────────────────────────────────────────────────────────

high_odds <- preds_with_odds %>%
  filter(implied_prob_A < 0.29)

cat("High-odds rows (implied_prob_A < 0.29):", nrow(high_odds), "\n")
cat("High-odds win rate:", round(mean(high_odds$outcome_num), 3), "\n")

# ── 5. Edge and P&L computation ───────────────────────────────────────────────

high_odds <- high_odds %>%
  mutate(
    edge_logistic = prob_logistic - implied_prob_A,
    edge_xgb      = prob_xgb      - implied_prob_A,
    won_bet       = outcome_num == 1
  )

# ── 6. Threshold sweep ────────────────────────────────────────────────────────

thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25)

simulate_flat <- function(df, edge_col, threshold) {
  bets <- df %>% filter(.data[[edge_col]] >= threshold)
  if (nrow(bets) == 0) {
    return(tibble(n_bets = 0, win_rate = NA, total_pnl = NA, roi = NA,
                  avg_odds = NA, avg_edge = NA))
  }
  bets %>%
    summarise(
      n_bets    = n(),
      win_rate  = mean(won_bet),
      total_pnl = sum(if_else(won_bet, odds_A - 1, -1)),
      roi       = total_pnl / n_bets,
      avg_odds  = mean(odds_A),
      avg_edge  = mean(.data[[edge_col]])
    )
}

results <- map_dfr(thresholds, function(thr) {
  log_res <- simulate_flat(high_odds, "edge_logistic", thr) %>%
    mutate(model = "Logistic (model_wform)", threshold = thr)
  xgb_res <- simulate_flat(high_odds, "edge_xgb",      thr) %>%
    mutate(model = "XGBoost",               threshold = thr)
  bind_rows(log_res, xgb_res)
}) %>%
  select(model, threshold, n_bets, win_rate, total_pnl, roi, avg_odds, avg_edge) %>%
  arrange(model, threshold)

# ── 7. Print and save ──────────────────────────────────────────────────────────

cat("\n========================================\n")
cat("HIGH-ODDS BACKTEST (implied_prob_A < 0.29)\n")
cat("Test period: 2023-2024 | Flat staking\n")
cat("========================================\n\n")
print(results, n = 30)

# Overall model metrics on full test set
cat("\n=== Model accuracy on full test set ===\n")
test_df_factor <- test_df %>% mutate(outcome = factor(outcome, levels = c("0","1")))

acc_log <- bind_cols(
  predict(model_wform,    test_df_factor, type = "prob"),
  predict(model_wform,    test_df_factor, type = "class"),
  test_df_factor %>% select(outcome)
) %>%
  summarise(
    accuracy = mean(.pred_class == outcome),
    brier    = mean((.pred_1 - as.numeric(as.character(outcome)))^2)
  ) %>%
  mutate(model = "Logistic")

acc_xgb <- bind_cols(
  predict(model_xgb_new, test_df_factor, type = "prob"),
  predict(model_xgb_new, test_df_factor, type = "class"),
  test_df_factor %>% select(outcome)
) %>%
  summarise(
    accuracy = mean(.pred_class == outcome),
    brier    = mean((.pred_1 - as.numeric(as.character(outcome)))^2)
  ) %>%
  mutate(model = "XGBoost")

cat("\n")
print(bind_rows(acc_log, acc_xgb) %>% select(model, accuracy, brier))

# Save results
out_path <- "C:/Users/User/OneDrive/Documents/tennis_model/backtest_results.csv"
write_csv(results, out_path)
cat("\nResults saved to:", out_path, "\n")

# Save the new XGBoost model into the environment snapshot
model_xgb_wform <- model_xgb_new
save(model_xgb_wform, file = "C:/Users/User/OneDrive/Documents/tennis_model/model_xgb_wform.rds")
cat("XGBoost model saved to model_xgb_wform.rds\n")
