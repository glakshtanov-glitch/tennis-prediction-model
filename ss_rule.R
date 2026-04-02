# ss_rule.R
# Straight-set streak rule: bet on (or against) players on a straight-set streak.
# A player is "on streak" when their last N tournament wins were all in straight sets.
# Depends on: startup.R, features.R, backtest.R

# ── Rule parameters ────────────────────────────────────────────────────────────

SS_MIN_STREAK  <- 2      # minimum streak to trigger the rule
SS_MIN_EDGE    <- 0.05   # minimum model edge to place a bet
SS_BET_ON      <- TRUE   # TRUE = bet on the streaking player; FALSE = bet against

# ── Filter to SS rule bets ─────────────────────────────────────────────────────
# Expects a data frame with ss_streakA, ss_streakB, edge, model_prob, odds_A columns.

apply_ss_rule <- function(df,
                           min_streak = SS_MIN_STREAK,
                           min_edge   = SS_MIN_EDGE,
                           bet_on     = SS_BET_ON) {
  df %>%
    filter(!is.na(ss_streakA), !is.na(ss_streakB)) %>%
    mutate(
      has_streak_A   = ss_streakA >= min_streak,
      has_streak_B   = ss_streakB >= min_streak,
      streak_diff    = ss_streakA - ss_streakB,
      trigger        = has_streak_A | has_streak_B
    ) %>%
    filter(trigger) %>%
    filter(!is.na(edge), abs(edge) >= min_edge) %>%
    mutate(
      # When bet_on = TRUE, back the player with the higher streak (playerA if streak_diff > 0)
      bet_on_A = if_else(bet_on, streak_diff > 0, streak_diff < 0),
      # Recalculate P&L: positive if we backed the right player
      won_bet  = if_else(bet_on_A,
                         as.numeric(as.character(outcome)) == 1,
                         as.numeric(as.character(outcome)) == 0),
      pnl_raw  = if_else(won_bet,
                         if_else(bet_on_A, odds_A - 1, (1 / (1 - 1/odds_A)) - 1),
                         -1)
    )
}

# ── SS rule summary ────────────────────────────────────────────────────────────

ss_rule_summary <- function(ss_bets, by_streak = FALSE) {
  if (by_streak) {
    ss_bets %>%
      mutate(effective_streak = pmax(ss_streakA, ss_streakB)) %>%
      group_by(effective_streak) %>%
      summarise(
        n_bets   = n(),
        win_rate = mean(won_bet),
        total_pnl = sum(pnl_raw),
        roi      = sum(pnl_raw) / n(),
        avg_odds  = mean(odds_A, na.rm = TRUE),
        .groups  = "drop"
      )
  } else {
    ss_bets %>%
      summarise(
        n_bets    = n(),
        win_rate  = mean(won_bet),
        total_pnl = sum(pnl_raw),
        roi       = sum(pnl_raw) / n(),
        avg_odds  = mean(odds_A, na.rm = TRUE),
        avg_edge  = mean(abs(edge), na.rm = TRUE)
      )
  }
}

# ── Streak distribution check ──────────────────────────────────────────────────

streak_distribution <- function(features_df) {
  bind_rows(
    features_df %>% select(player = playerA, ss_streak = ss_streakA,
                            tourney_date, tourney_id),
    features_df %>% select(player = playerB, ss_streak = ss_streakB,
                            tourney_date, tourney_id)
  ) %>%
    filter(!is.na(ss_streak), ss_streak > 0) %>%
    count(ss_streak, name = "n_instances") %>%
    arrange(ss_streak)
}

# ── Full SS backtest ───────────────────────────────────────────────────────────

run_ss_backtest <- function(model_fit, features_df, odds_all_df,
                             min_streak = SS_MIN_STREAK,
                             min_edge   = SS_MIN_EDGE) {
  # Get full predictions + odds (reuses backtest.R helpers)
  preds_with_odds <- run_backtest(
    model_fit   = model_fit,
    features_df = features_df,
    odds_all_df = odds_all_df,
    min_edge    = 0          # no edge filter yet — SS rule filters separately
  )$all_preds

  ss_bets <- apply_ss_rule(preds_with_odds,
                            min_streak = min_streak,
                            min_edge   = min_edge)

  list(
    bets          = ss_bets,
    summary       = ss_rule_summary(ss_bets),
    by_streak     = ss_rule_summary(ss_bets, by_streak = TRUE),
    distribution  = streak_distribution(features_df)
  )
}

# ── Interactive run ────────────────────────────────────────────────────────────

if (FALSE) {
  source("startup.R")
  source("features.R")
  source("model.R")
  source("backtest.R")

  result <- run_ss_backtest(
    model_fit   = model_wform,
    features_df = df_features6,
    odds_all_df = odds_all,
    min_streak  = 2,
    min_edge    = 0.05
  )

  cat("\n=== SS Rule Summary ===\n")
  print(result$summary)

  cat("\n=== By Streak Length ===\n")
  print(result$by_streak)

  cat("\n=== Streak Distribution in Data ===\n")
  print(result$distribution)

  # Compare to bets_ss already in environment (pre-computed)
  cat("\n=== bets_ss (existing) summary ===\n")
  bets_ss %>%
    summarise(n = n(), win_rate = mean(won_bet), roi = sum(pnl_raw)/n())
}
