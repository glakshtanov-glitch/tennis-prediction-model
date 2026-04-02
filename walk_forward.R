# walk_forward.R
# Walk-forward validation of model_inter3_elo across 5 rolling windows.
# For each window: retrain on growing history, test on held-out year(s),
# run high-odds backtest (implied < 0.29, edge >= 0.15).
#
# Windows:
#   W1: train 2015-2018  |  test 2019
#   W2: train 2015-2019  |  test 2020
#   W3: train 2015-2020  |  test 2021
#   W4: train 2015-2021  |  test 2022
#   W5: train 2015-2022  |  test 2023-2024

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

elo_lookup <- readRDS("elo_surface_lookup.rds")

# ── Helper: extract 4-digit year from YYYYMMDD integer ───────────────────────
td_year <- function(x) as.integer(substr(as.character(x), 1, 4))

# ── Full feature table with Elo ───────────────────────────────────────────────
# train_inter (2015-2019) and test_inter (2020-2024) together span the full
# period and already have log_rank_ratio + games_dom_x_underdog computed.
# Combine them, add elo_diff_surface, then re-slice by year per window.
cat("Building full feature table with Elo...\n")

all_inter <- bind_rows(
  train_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  test_inter  %>% mutate(outcome = factor(outcome, levels = c("0","1")))
)

full_df <- add_elo_feature(all_inter, elo_lookup) %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface)) %>%
  mutate(year = td_year(tourney_date))

cat(sprintf("Full dataset: %d rows  |  years %d-%d\n\n",
            nrow(full_df), min(full_df$year), max(full_df$year)))

# ── Odds lookup (same join logic used throughout project) ────────────────────
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

join_odds <- function(feat_df) {
  prep <- feat_df %>%
    mutate(
      playerA_abbrev = map_chr(playerA, abbreviate_name),
      playerB_abbrev = map_chr(playerB, abbreviate_name),
      surface_clean  = as.character(surface),
      td_str         = as.character(tourney_date),
      date_key       = paste0(substr(td_str,1,4), "-", substr(td_str,5,6))
    )

  a_wins <- prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="winner_abbrev","playerB_abbrev"="loser_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365W, implied_prob_A = norm_prob_W)

  a_loses <- prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="loser_abbrev","playerB_abbrev"="winner_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365L, implied_prob_A = norm_prob_L)

  bind_rows(a_wins, a_loses) %>%
    mutate(outcome_num = as.integer(as.character(outcome)),
           won_bet     = outcome_num == 1) %>%
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# ── Significance: Poisson-binomial normal approximation ──────────────────────
sig_test <- function(bets) {
  n     <- nrow(bets)
  if (n == 0) return(list(z = NA_real_, p = NA_real_))
  wins  <- sum(bets$won_bet)
  ps    <- bets$implied_prob_A
  z     <- (wins - sum(ps)) / sqrt(sum(ps * (1 - ps)))
  p     <- pnorm(z, lower.tail = FALSE)
  list(z = round(z, 3), p = round(p, 5))
}

# ── Walk-forward windows ──────────────────────────────────────────────────────
windows <- list(
  list(label = "W1: train 2015-18 | test 2019",      train_max = 2018, test_min = 2019, test_max = 2019),
  list(label = "W2: train 2015-19 | test 2020",      train_max = 2019, test_min = 2020, test_max = 2020),
  list(label = "W3: train 2015-20 | test 2021",      train_max = 2020, test_min = 2021, test_max = 2021),
  list(label = "W4: train 2015-21 | test 2022",      train_max = 2021, test_min = 2022, test_max = 2022),
  list(label = "W5: train 2015-22 | test 2023-2024", train_max = 2022, test_min = 2023, test_max = 2024)
)

FORMULA   <- outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface
EDGE_THR  <- 0.15
HO_THR    <- 0.29

results <- map_dfr(windows, function(w) {
  cat(sprintf("Running %s...\n", w$label))

  train_df <- full_df %>% filter(year >= 2015, year <= w$train_max)
  test_df  <- full_df %>% filter(year >= w$test_min, year <= w$test_max)

  cat(sprintf("  train: %d rows  |  test: %d rows\n", nrow(train_df), nrow(test_df)))

  # Retrain
  model <- logistic_reg() %>% set_engine("glm") %>% fit(FORMULA, data = train_df)

  # Accuracy metrics on full test set
  p   <- predict(model, test_df, type = "prob")$.pred_1
  y   <- as.integer(as.character(test_df$outcome))
  auc <- {
    r <- rank(p); n1 <- sum(y==1); n0 <- sum(y==0)
    (sum(r[y==1]) - n1*(n1+1)/2) / (n1*n0)
  }
  brier <- mean((p - y)^2)

  # Attach predictions, join odds
  test_preds <- test_df %>% mutate(prob = p)
  with_odds  <- join_odds(test_preds) %>%
    mutate(edge = prob - implied_prob_A)

  ho   <- with_odds %>% filter(implied_prob_A < HO_THR)
  bets <- ho %>% filter(edge >= EDGE_THR)
  n    <- nrow(bets)

  if (n > 0) {
    wr  <- mean(bets$won_bet)
    roi <- sum(if_else(bets$won_bet, bets$odds_A - 1, -1)) / n
    sig <- sig_test(bets)
  } else {
    wr  <- NA_real_; roi <- NA_real_
    sig <- list(z = NA_real_, p = NA_real_)
  }

  cat(sprintf("  high-odds matched: %d  |  bets at edge>=%.2f: %d\n",
              nrow(ho), EDGE_THR, n))

  tibble(
    window          = w$label,
    train_years     = sprintf("2015-%d", w$train_max),
    test_years      = sprintf("%d-%d", w$test_min, w$test_max),
    train_n         = nrow(train_df),
    test_n          = nrow(test_df),
    auc             = round(auc,   4),
    brier           = round(brier, 4),
    ho_matched      = nrow(ho),
    n_bets          = n,
    win_rate        = round(wr,  4),
    avg_implied     = if (n>0) round(mean(bets$implied_prob_A), 4) else NA_real_,
    avg_odds        = if (n>0) round(mean(bets$odds_A), 2) else NA_real_,
    roi             = round(roi, 4),
    z_score         = sig$z,
    p_value         = sig$p,
    significant     = if (!is.na(sig$p)) sig$p < 0.05 else NA
  )
})

# ── Print full results ────────────────────────────────────────────────────────
cat("\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("WALK-FORWARD VALIDATION  |  model_inter3_elo  |  edge >= %.2f  |  implied < %.2f\n",
            EDGE_THR, HO_THR))
cat("══════════════════════════════════════════════════════════════════════════\n\n")

cat("── Model accuracy ───────────────────────────────────────────────────────\n\n")
results %>%
  select(window, train_n, test_n, auc, brier) %>%
  print(n = 10)

cat("\n── Backtest results ─────────────────────────────────────────────────────\n\n")
results %>%
  select(window, n_bets, win_rate, avg_implied, avg_odds, roi, z_score, p_value, significant) %>%
  print(n = 10)

cat("\n── Cumulative picture ───────────────────────────────────────────────────\n")
total_bets <- sum(results$n_bets, na.rm = TRUE)
total_wins <- sum(results$n_bets * results$win_rate, na.rm = TRUE)
total_roi  <- results %>%
  filter(!is.na(roi)) %>%
  summarise(roi = weighted.mean(roi, n_bets)) %>%
  pull(roi)
sig_windows <- sum(results$significant, na.rm = TRUE)

cat(sprintf("  Total bets across all windows : %d\n", total_bets))
cat(sprintf("  Weighted-avg win rate         : %.1f%%\n", total_wins/total_bets*100))
cat(sprintf("  Weighted-avg ROI              : %.1f%%\n", total_roi*100))
cat(sprintf("  Windows with p < 0.05         : %d / %d\n", sig_windows, nrow(results)))

# ── Save ──────────────────────────────────────────────────────────────────────
write_csv(results, "walk_forward_results.csv")
cat("\nSaved walk_forward_results.csv\n")
