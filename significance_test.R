setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

model_inter3_elo <- readRDS("model_inter3_elo.rds")
elo_lookup       <- readRDS("elo_surface_lookup.rds")

# ── Rebuild test predictions (identical to elo_retrain_backtest.R) ────────────
cat("Rebuilding test predictions...\n")

test_inter_elo <- add_elo_feature(
  test_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
)

test_clean_elo <- test_inter_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))

preds_elo <- predict(model_inter3_elo, test_clean_elo, type = "prob") %>%
  rename(prob = .pred_1) %>%
  bind_cols(test_clean_elo)

# ── Odds join (same logic as compare_backtest.R) ──────────────────────────────
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

preds_with_odds <- bind_rows(
  preds_elo %>%
    mutate(playerA_abbrev = map_chr(playerA, abbreviate_name),
           playerB_abbrev = map_chr(playerB, abbreviate_name),
           surface_clean  = as.character(surface),
           date_key = paste0(substr(as.character(tourney_date),1,4), "-",
                             substr(as.character(tourney_date),5,6))) %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="winner_abbrev","playerB_abbrev"="loser_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365W, implied_prob_A = norm_prob_W),

  preds_elo %>%
    mutate(playerA_abbrev = map_chr(playerA, abbreviate_name),
           playerB_abbrev = map_chr(playerB, abbreviate_name),
           surface_clean  = as.character(surface),
           date_key = paste0(substr(as.character(tourney_date),1,4), "-",
                             substr(as.character(tourney_date),5,6))) %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="loser_abbrev","playerB_abbrev"="winner_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365L, implied_prob_A = norm_prob_L)
) %>%
  mutate(outcome_num = as.integer(as.character(outcome)),
         won_bet     = outcome_num == 1,
         edge        = prob - implied_prob_A) %>%
  group_by(match_id) %>%
  slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
  ungroup()

high_odds <- preds_with_odds %>% filter(implied_prob_A < 0.29)
cat("High-odds bets available:", nrow(high_odds), "\n\n")

# ── Significance test function ────────────────────────────────────────────────
# Two complementary approaches:
#
# 1. Simple binom.test  — H0: win_rate = mean(implied_prob) across the subset.
#    Clean and easy to interpret but treats all bets as having equal null prob.
#
# 2. Poisson-binomial normal approximation — H0: each bet wins with its own
#    individual implied_prob_i. Correct when implied probs vary across bets.
#    E[W] = sum(p_i),  Var[W] = sum(p_i*(1-p_i)),  z = (W - E[W]) / sd(W)

run_significance <- function(bets_df, threshold, alpha = 0.05) {
  bets    <- bets_df %>% filter(edge >= threshold)
  n       <- nrow(bets)
  wins    <- sum(bets$won_bet)
  wr      <- wins / n
  ps      <- bets$implied_prob_A          # individual market-implied probs
  p_avg   <- mean(ps)

  if (n == 0) return(NULL)

  # ── 1. Simple binomial test (H1: win rate > mean implied prob) ─────────────
  bt <- binom.test(wins, n, p = p_avg, alternative = "greater", conf.level = 0.95)

  # ── 2. Poisson-binomial normal approximation ───────────────────────────────
  exp_wins <- sum(ps)
  var_wins <- sum(ps * (1 - ps))
  sd_wins  <- sqrt(var_wins)
  z        <- (wins - exp_wins) / sd_wins
  p_pb     <- pnorm(z, lower.tail = FALSE)          # one-sided p-value

  # 95% CI on win rate via Wilson score interval
  z95   <- qnorm(0.975)
  denom <- 1 + z95^2 / n
  centre <- (wr + z95^2 / (2*n)) / denom
  margin <- z95 * sqrt(wr*(1-wr)/n + z95^2/(4*n^2)) / denom
  ci_lo <- centre - margin
  ci_hi <- centre + margin

  # Expected ROI under null (market-fair)
  exp_roi_null <- mean(ps * (bets$odds_A - 1) + (1 - ps) * (-1))

  tibble(
    threshold      = threshold,
    n_bets         = n,
    wins           = wins,
    win_rate       = round(wr, 4),
    avg_implied    = round(p_avg, 4),
    edge_over_mkt  = round(wr - p_avg, 4),
    # Binomial test
    p_binom        = round(bt$p.value, 5),
    # Poisson-binomial approximation
    z_score        = round(z, 3),
    p_poisson_binom = round(p_pb, 5),
    # Wilson CI on win rate
    ci_95_lo       = round(ci_lo, 4),
    ci_95_hi       = round(ci_hi, 4),
    # Implied ROI if win rate held at CI bounds
    roi_observed   = round((wins * mean(bets$odds_A[bets$won_bet]) -
                              (n - wins) - wins) / n, 4),
    roi_at_ci_lo   = round(ci_lo * mean(bets$odds_A) - 1, 4),
    roi_at_ci_hi   = round(ci_hi * mean(bets$odds_A) - 1, 4),
    significant    = p_pb < alpha
  )
}

thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25)
results    <- map_dfr(thresholds, ~ run_significance(high_odds, .x))

# ── Print ─────────────────────────────────────────────────────────────────────
cat("══════════════════════════════════════════════════════════════════════\n")
cat("SIGNIFICANCE TEST — model_inter3_elo on high-odds subset (implied < 0.29)\n")
cat("H0: true win rate = market-implied win rate  |  H1: win rate > implied\n")
cat("Test period: 2020-2024  |  One-sided  |  α = 0.05\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

cat("── Core results ──────────────────────────────────────────────────────\n")
results %>%
  select(threshold, n_bets, wins, win_rate, avg_implied, edge_over_mkt,
         z_score, p_poisson_binom, p_binom, significant) %>%
  print(n = 10)

cat("\n── 95% Wilson confidence intervals on win rate ───────────────────────\n")
results %>%
  select(threshold, n_bets, win_rate, avg_implied, ci_95_lo, ci_95_hi) %>%
  mutate(implied_inside_ci = avg_implied >= ci_95_lo & avg_implied <= ci_95_hi) %>%
  print(n = 10)

cat("\n── ROI confidence bounds (flat staking, avg odds per threshold) ──────\n")
results %>%
  select(threshold, n_bets, roi_observed, roi_at_ci_lo, roi_at_ci_hi,
         p_poisson_binom, significant) %>%
  print(n = 10)

cat("\n── Interpretation ────────────────────────────────────────────────────\n")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  sig_str <- if (r$significant) "SIGNIFICANT" else "not significant"
  cat(sprintf(
    "  edge>=%.2f : n=%d  win=%.1f%%  implied=%.1f%%  z=%.2f  p=%.4f  [%s]\n",
    r$threshold, r$n_bets, r$win_rate*100, r$avg_implied*100,
    r$z_score, r$p_poisson_binom, sig_str
  ))
}
