# model.R
# Model training, evaluation, and comparison.
# Depends on: startup.R, features.R

# ── Train/test split ───────────────────────────────────────────────────────────
# Temporal split: train on earlier years, test on most recent year.

make_split <- function(features_df, test_year = 2024) {
  features_df <- features_df %>%
    mutate(year = as.integer(substr(as.character(tourney_date), 1, 4)),
           outcome = factor(outcome, levels = c(0, 1)))

  train_df <- features_df %>% filter(year < test_year)
  test_df  <- features_df %>% filter(year == test_year)
  list(train = train_df, test = test_df)
}

# ── Model specifications ───────────────────────────────────────────────────────

spec_logistic <- logistic_reg() %>% set_engine("glm")
spec_xgb      <- boost_tree(trees = 500, learn_rate = 0.05, tree_depth = 4) %>%
                   set_engine("xgboost") %>%
                   set_mode("classification")

# ── Recipes (feature sets) ─────────────────────────────────────────────────────

recipe_wform <- function(train_df) {
  recipe(outcome ~ log_rank_ratio + wform_diff + surf_wr_diff + h2h_v3 +
           fatigue_diff + ss_streak_diff + games_dom_diff,
         data = train_df) %>%
    step_impute_median(all_numeric_predictors())
}

recipe_best <- function(train_df) {
  recipe(outcome ~ log_rank_ratio + surf_wr_diff + h2h_v3,
         data = train_df) %>%
    step_impute_median(all_numeric_predictors())
}

# ── Fit helpers ────────────────────────────────────────────────────────────────

fit_model <- function(spec, recipe, train_df) {
  workflow() %>%
    add_recipe(recipe) %>%
    add_model(spec) %>%
    fit(data = train_df)
}

# ── Evaluation ────────────────────────────────────────────────────────────────

evaluate_model <- function(model_fit, test_df) {
  preds <- predict(model_fit, test_df, type = "prob") %>%
    bind_cols(test_df %>% select(outcome, playerA, playerB, tourney_date))

  preds %>%
    mutate(
      pred_class = factor(if_else(.pred_1 >= 0.5, 1, 0), levels = c(0, 1)),
      outcome    = factor(outcome, levels = c(0, 1))
    ) %>%
    {
      metrics_df <- bind_rows(
        accuracy(., outcome, pred_class),
        roc_auc(., outcome, .pred_1)
      )
      brier <- mean((.$.pred_1 - as.numeric(as.character(.$outcome)))^2)
      list(
        metrics = metrics_df,
        brier   = brier,
        preds   = .
      )
    }
}

print_metrics <- function(eval_result, label = "") {
  cat("\n===", label, "===\n")
  print(eval_result$metrics)
  cat("Brier score:", round(eval_result$brier, 4), "\n")
}

# ── Calibration check ──────────────────────────────────────────────────────────

calibration_plot <- function(preds_df, bins = 10) {
  preds_df %>%
    mutate(bin = cut(.pred_1, breaks = seq(0, 1, length.out = bins + 1),
                     include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(.pred_1),
      actual    = mean(as.numeric(as.character(outcome))),
      n         = n(),
      .groups   = "drop"
    ) %>%
    ggplot(aes(x = mean_pred, y = actual)) +
    geom_point(aes(size = n)) +
    geom_abline(linetype = "dashed") +
    labs(title = "Calibration plot", x = "Predicted probability", y = "Actual win rate") +
    theme_minimal()
}

# ── Main training run ──────────────────────────────────────────────────────────
# Uncomment and run interactively to retrain.

if (FALSE) {
  source("startup.R")
  source("features.R")

  splits   <- make_split(df_features6)
  train_df <- splits$train
  test_df  <- splits$test

  # Logistic – weighted form feature set
  model_wform <- fit_model(spec_logistic, recipe_wform(train_df), train_df)
  eval_wform  <- evaluate_model(model_wform, test_df)
  print_metrics(eval_wform, "model_wform")

  # Logistic – best (parsimonious) feature set
  model_best <- fit_model(spec_logistic, recipe_best(train_df), train_df)
  eval_best  <- evaluate_model(model_best, test_df)
  print_metrics(eval_best, "model_best")

  # XGBoost
  model_xgb <- fit_model(spec_xgb, recipe_wform(train_df), train_df)
  eval_xgb  <- evaluate_model(model_xgb, test_df)
  print_metrics(eval_xgb, "model_xgb")

  # Save updated environment
  save.image(file = "C:/Users/User/OneDrive/Documents/tennis_project.RData")
}
