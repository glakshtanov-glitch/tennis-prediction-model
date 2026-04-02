# backtest.R
# Backtesting model predictions against historical Bet365 odds (2020-2024).
# Depends on: startup.R, features.R, model.R

# ── Odds loading ───────────────────────────────────────────────────────────────

read_odds <- function(path) {
  read_csv(path, col_types = cols(
    WRank = col_character(), LRank = col_character(),
    WPts  = col_character(), LPts  = col_character()
  )) %>%
    mutate(
      WRank = as.numeric(WRank),
      LRank = as.numeric(LRank),
      WPts  = as.numeric(WPts),
      LPts  = as.numeric(LPts)
    )
}

# ── Join predictions with odds ─────────────────────────────────────────────────
# features_df must already contain model probabilities (.pred_1) from predict().
# odds_df is the raw Bet365 file with columns: Winner, Loser, B365W, B365L.

join_odds <- function(preds_df, odds_df) {
  odds_clean <- odds_df %>%
    transmute(
      playerA_abbrev = map_chr(Winner, abbreviate_name),
      playerB_abbrev = map_chr(Loser,  abbreviate_name),
      Surface        = Surface,
      WRank, LRank,
      B365W, B365L
    )

  preds_df %>%
    mutate(
      playerA_abbrev = map_chr(playerA, abbreviate_name),
      playerB_abbrev = map_chr(playerB, abbreviate_name)
    ) %>%
    left_join(odds_clean,
              by = c("playerA_abbrev", "playerB_abbrev", "Surface")) %>%
    mutate(
      implied_prob_W = 1 / B365W,
      implied_prob_L = 1 / B365L,
      # Normalise for overround
      implied_prob_A = implied_prob_W / (implied_prob_W + implied_prob_L),
      odds_A         = B365W
    )
}

# ── Edge calculation ───────────────────────────────────────────────────────────
# edge = model_prob - implied_prob_A  (positive = model thinks playerA is underpriced)

add_edge <- function(df) {
  df %>% mutate(
    model_prob = .pred_1,
    edge       = model_prob - implied_prob_A,
    outcome_num = as.numeric(as.character(outcome))
  )
}

# ── Betting simulation ─────────────────────────────────────────────────────────
# Flat staking on all bets where edge >= min_edge.
# Returns per-bet P&L (stake = 1 unit).

simulate_bets <- function(df, min_edge = 0.05) {
  df %>%
    filter(!is.na(edge), !is.na(odds_A), edge >= min_edge) %>%
    mutate(
      won_bet = outcome_num == 1,
      pnl_raw = if_else(won_bet, odds_A - 1, -1),
      is_underdog = implied_prob_A < 0.5,
      upset       = is_underdog & won_bet,
      non_upset   = !is_underdog & won_bet
    )
}

# ── Summary metrics ────────────────────────────────────────────────────────────

backtest_summary <- function(bets_df, label = "") {
  bets_df %>%
    summarise(
      label       = label,
      n_bets      = n(),
      win_rate    = mean(won_bet),
      total_pnl   = sum(pnl_raw),
      roi         = sum(pnl_raw) / n(),
      avg_odds    = mean(odds_A, na.rm = TRUE),
      avg_edge    = mean(edge, na.rm = TRUE),
      .groups     = "drop"
    )
}

# ── Full backtest pipeline ─────────────────────────────────────────────────────

run_backtest <- function(model_fit, features_df, odds_all_df,
                          min_edge = 0.05, test_year = 2024) {
  test_df <- features_df %>%
    filter(as.integer(substr(as.character(tourney_date), 1, 4)) == test_year) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1)))

  preds <- predict(model_fit, test_df, type = "prob") %>%
    bind_cols(test_df)

  preds_with_odds <- join_odds(preds, odds_all_df) %>%
    add_edge()

  bets <- simulate_bets(preds_with_odds, min_edge = min_edge)

  list(
    all_preds = preds_with_odds,
    bets      = bets,
    summary   = backtest_summary(bets, label = paste0("edge>=", min_edge))
  )
}

# ── Interactive run ────────────────────────────────────────────────────────────

if (FALSE) {
  source("startup.R")
  source("features.R")
  source("model.R")

  result <- run_backtest(
    model_fit    = model_wform,
    features_df  = df_features6,
    odds_all_df  = odds_all,
    min_edge     = 0.05,
    test_year    = 2024
  )

  print(result$summary)

  # Sweep over edge thresholds
  thresholds <- seq(0.02, 0.20, by = 0.02)
  sweep_results <- map_dfr(thresholds, function(e) {
    bets <- simulate_bets(result$all_preds, min_edge = e)
    backtest_summary(bets, label = paste0("edge>=", e))
  })
  print(sweep_results)
}
