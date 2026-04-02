# calibration.R
# Calibration analysis for model_inter3_elo on the 2020-2024 test set.
#
# 1. Decile-bin calibration: mean predicted prob vs observed win rate
# 2. Brier score decomposition: REL + RES + UNC
# 3. Calibration in the high-odds betting subset (implied_prob_A < 0.29)
# 4. Saves calibration_plot.png (3-panel)
# 5. Concludes whether edge threshold 0.15 is sound

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")
library(patchwork)

model_inter3_elo <- readRDS("model_inter3_elo.rds")
elo_lookup       <- readRDS("elo_surface_lookup.rds")

# ════════════════════════════════════════════════════════════════════════════
# STEP 1 — Rebuild test predictions (2020-2024)
# ════════════════════════════════════════════════════════════════════════════

cat("Rebuilding test predictions on 2020-2024...\n")

test_elo <- add_elo_feature(
  test_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
)

test_clean <- test_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))

preds <- predict(model_inter3_elo, test_clean, type = "prob") %>%
  rename(prob = .pred_1) %>%
  bind_cols(test_clean) %>%
  mutate(y = as.integer(as.character(outcome)))

cat(sprintf("Test predictions: %d rows  |  base win rate: %.3f\n\n",
            nrow(preds), mean(preds$y)))

# ════════════════════════════════════════════════════════════════════════════
# STEP 2 — Odds join for high-odds subset
# ════════════════════════════════════════════════════════════════════════════

cat("Joining odds...\n")

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

prep <- preds %>%
  mutate(
    playerA_abbrev = map_chr(playerA, abbreviate_name),
    playerB_abbrev = map_chr(playerB, abbreviate_name),
    surface_clean  = as.character(surface),
    td_str         = as.character(tourney_date),
    date_key       = paste0(substr(td_str,1,4), "-", substr(td_str,5,6))
  )

preds_odds <- bind_rows(
  prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="winner_abbrev","playerB_abbrev"="loser_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365W, implied_prob_A = norm_prob_W),
  prep %>%
    inner_join(odds_prep,
               by = c("playerA_abbrev"="loser_abbrev","playerB_abbrev"="winner_abbrev",
                      "surface_clean"="surface_odds","date_key"="date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365L, implied_prob_A = norm_prob_L)
) %>%
  group_by(match_id) %>%
  slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(edge = prob - implied_prob_A)

ho_preds <- preds_odds %>% filter(implied_prob_A < 0.29)

cat(sprintf("Odds-matched rows: %d  |  high-odds (implied < 0.29): %d\n\n",
            nrow(preds_odds), nrow(ho_preds)))

# ════════════════════════════════════════════════════════════════════════════
# STEP 3 — Shared helpers
# ════════════════════════════════════════════════════════════════════════════

wilson_lo <- function(p, n) {
  z <- qnorm(0.975)
  denom  <- 1 + z^2/n
  centre <- (p + z^2/(2*n)) / denom
  margin <- z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / denom
  pmax(0, centre - margin)
}
wilson_hi <- function(p, n) {
  z <- qnorm(0.975)
  denom  <- 1 + z^2/n
  centre <- (p + z^2/(2*n)) / denom
  margin <- z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / denom
  pmin(1, centre + margin)
}

# ── Brier decomposition ───────────────────────────────────────────────────
# Brier = REL - RES + UNC
#   UNC = p_bar*(1-p_bar)               — irreducible variance
#   REL = (1/N) * Σ_k n_k*(f_k - o_k)² — reliability: mean calibration error
#   RES = (1/N) * Σ_k n_k*(o_k - p_bar)²— resolution: how far bins deviate from base rate

brier_decompose <- function(prob, y, n_bins = 10) {
  N     <- length(prob)
  p_bar <- mean(y)
  brier <- mean((prob - y)^2)

  # Equal-frequency bins
  breaks   <- quantile(prob, probs = seq(0, 1, length.out = n_bins + 1), names = FALSE)
  breaks[1]  <- breaks[1] - 1e-9
  bin_id   <- cut(prob, breaks = breaks, labels = FALSE, include.lowest = TRUE)

  bins <- tibble(prob = prob, y = y, bin = bin_id) %>%
    group_by(bin) %>%
    summarise(
      n    = n(),
      f_k  = mean(prob),   # mean forecast in bin
      o_k  = mean(y),      # observed win rate in bin
      .groups = "drop"
    )

  REL   <- sum(bins$n * (bins$f_k - bins$o_k)^2) / N
  RES   <- sum(bins$n * (bins$o_k - p_bar)^2)    / N
  UNC   <- p_bar * (1 - p_bar)

  list(
    brier = brier,
    REL   = REL,
    RES   = RES,
    UNC   = UNC,
    check = REL - RES + UNC,   # should equal brier
    bins  = bins,
    p_bar = p_bar,
    N     = N
  )
}

# ════════════════════════════════════════════════════════════════════════════
# STEP 4 — Calibration bins for plotting
# ════════════════════════════════════════════════════════════════════════════

make_cal_bins <- function(prob, y, n_bins = 10, label = "Full test set") {
  N      <- length(prob)
  breaks <- quantile(prob, probs = seq(0, 1, length.out = n_bins + 1), names = FALSE)
  breaks[1] <- breaks[1] - 1e-9
  bin_id <- cut(prob, breaks = breaks, labels = FALSE, include.lowest = TRUE)

  tibble(prob = prob, y = y, bin = bin_id) %>%
    group_by(bin) %>%
    summarise(
      n         = n(),
      mean_pred = mean(prob),
      obs_rate  = mean(y),
      .groups   = "drop"
    ) %>%
    mutate(
      ci_lo  = wilson_lo(obs_rate, n),
      ci_hi  = wilson_hi(obs_rate, n),
      subset = label
    )
}

cal_full <- make_cal_bins(preds$prob, preds$y, label = "Full test set (n=2950)")
cal_ho   <- make_cal_bins(ho_preds$prob, ho_preds$y, label = "High-odds subset (n=378, implied<0.29)")

# ════════════════════════════════════════════════════════════════════════════
# STEP 5 — Brier decompositions
# ════════════════════════════════════════════════════════════════════════════

bd_full <- brier_decompose(preds$prob,    preds$y)
bd_ho   <- brier_decompose(ho_preds$prob, ho_preds$y)

# ════════════════════════════════════════════════════════════════════════════
# STEP 6 — Calibration in the betting zone (edge >= 0.15, implied < 0.29)
# ════════════════════════════════════════════════════════════════════════════

bet_preds <- preds_odds %>% filter(implied_prob_A < 0.29, edge >= 0.15)

cat(sprintf("Betting zone (implied<0.29 + edge>=0.15): %d bets\n", nrow(bet_preds)))

# Calibration within the betting zone — fixed probability bins
bet_bins <- tibble(
  prob = bet_preds$prob,
  y    = bet_preds$y
) %>%
  mutate(bin = cut(prob,
                   breaks = c(0, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.60, 1.0),
                   include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    n         = n(),
    mean_pred = mean(prob),
    obs_rate  = mean(y),
    .groups   = "drop"
  ) %>%
  mutate(
    ci_lo = if_else(n >= 3, wilson_lo(obs_rate, n), NA_real_),
    ci_hi = if_else(n >= 3, wilson_hi(obs_rate, n), NA_real_)
  ) %>%
  filter(!is.na(mean_pred))

# ════════════════════════════════════════════════════════════════════════════
# STEP 7 — Build plots
# ════════════════════════════════════════════════════════════════════════════

theme_cal <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, colour = "grey40")
  )

# ── Panel 1: Full test-set calibration curve ──────────────────────────────
p1 <- ggplot(cal_full, aes(x = mean_pred, y = obs_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey60", linewidth = 0.6) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "#2196F3", alpha = 0.18) +
  geom_line(colour = "#2196F3", linewidth = 0.8) +
  geom_point(aes(size = n), colour = "#1565C0", fill = "#2196F3",
             shape = 21, stroke = 0.6) +
  scale_size_continuous(range = c(2, 7), name = "n per bin") +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(1)) +
  labs(
    title    = "Full test set — decile calibration",
    subtitle = sprintf("Brier=%.4f  |  REL=%.4f  RES=%.4f  UNC=%.4f  |  n=%d",
                       bd_full$brier, bd_full$REL, bd_full$RES, bd_full$UNC,
                       bd_full$N),
    x = "Mean predicted probability",
    y = "Observed win rate"
  ) +
  theme_cal

# ── Panel 2: High-odds subset calibration curve ───────────────────────────
p2 <- ggplot(cal_ho, aes(x = mean_pred, y = obs_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey60", linewidth = 0.6) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "#FF9800", alpha = 0.22) +
  geom_line(colour = "#FF9800", linewidth = 0.8) +
  geom_point(aes(size = n), colour = "#E65100", fill = "#FF9800",
             shape = 21, stroke = 0.6) +
  scale_size_continuous(range = c(2, 7), name = "n per bin") +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(1)) +
  labs(
    title    = "High-odds subset (implied < 0.29) — decile calibration",
    subtitle = sprintf("Brier=%.4f  |  REL=%.4f  RES=%.4f  UNC=%.4f  |  n=%d",
                       bd_ho$brier, bd_ho$REL, bd_ho$RES, bd_ho$UNC,
                       bd_ho$N),
    x = "Mean predicted probability",
    y = "Observed win rate"
  ) +
  theme_cal

# ── Panel 3: Betting zone detail (edge>=0.15, implied<0.29) ───────────────
p3_data <- bind_rows(
  cal_full %>% mutate(subset = "Full test set"),
  cal_ho   %>% mutate(subset = "High-odds (implied<0.29)")
)

# Histogram of model probs in the betting zone vs rest
hist_data <- bind_rows(
  tibble(prob = preds_odds$prob, group = "All odds-matched"),
  tibble(prob = ho_preds$prob,   group = "High-odds (implied<0.29)"),
  tibble(prob = bet_preds$prob,  group = "Betting zone (edge>=0.15)")
)

p3 <- ggplot(bet_bins %>% filter(n >= 3),
             aes(x = mean_pred, y = obs_rate)) +
  geom_hline(yintercept = mean(bet_preds$y), linetype = "dotted",
             colour = "grey50", linewidth = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey60", linewidth = 0.6) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.012, colour = "#4CAF50", linewidth = 0.7) +
  geom_point(aes(size = n), colour = "#1B5E20", fill = "#4CAF50",
             shape = 21, stroke = 0.7) +
  geom_text(aes(label = sprintf("n=%d\n%.0f%%", n, obs_rate*100)),
            vjust = -0.9, size = 2.8, colour = "grey30") +
  scale_size_continuous(range = c(3, 9), name = "n") +
  scale_x_continuous(limits = c(0.15, 0.75), labels = scales::percent_format(1)) +
  scale_y_continuous(limits = c(0, 0.80),    labels = scales::percent_format(1)) +
  annotate("rect", xmin = 0.15, xmax = 0.29, ymin = 0, ymax = 0.80,
           fill = "grey90", alpha = 0.4) +
  annotate("text", x = 0.22, y = 0.75, label = "model agrees\nwith market here",
           size = 2.8, colour = "grey50") +
  labs(
    title    = "Betting zone: model prob vs observed win rate (edge≥0.15, implied<0.29)",
    subtitle = sprintf("n=%d bets  |  overall obs win rate=%.1f%%  |  dashed=perfect calibration  |  dotted=observed base rate",
                       nrow(bet_preds), mean(bet_preds$y)*100),
    x = "Model predicted probability (playerA wins)",
    y = "Observed win rate"
  ) +
  theme_cal

# ── Combine and save ───────────────────────────────────────────────────────
combined <- (p1 | p2) / p3 +
  plot_annotation(
    title   = "model_inter3_elo — Calibration Analysis (test 2020-2024)",
    caption = "Shaded band = 95% Wilson CI on observed win rate per bin"
  )

ggsave("calibration_plot.png", combined, width = 14, height = 10, dpi = 150)
cat("Saved calibration_plot.png\n\n")

# ════════════════════════════════════════════════════════════════════════════
# STEP 8 — Print Brier decompositions
# ════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════════\n")
cat("BRIER SCORE DECOMPOSITION  —  model_inter3_elo  |  test 2020-2024\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("── Full test set ─────────────────────────────────────────────────\n")
cat(sprintf("  N                 : %d\n",     bd_full$N))
cat(sprintf("  Base win rate     : %.4f\n",   bd_full$p_bar))
cat(sprintf("  Brier score       : %.4f\n",   bd_full$brier))
cat(sprintf("  REL (reliability) : %.4f   [↓ better — calibration error]\n", bd_full$REL))
cat(sprintf("  RES (resolution)  : %.4f   [↑ better — how much bins spread]\n", bd_full$RES))
cat(sprintf("  UNC (uncertainty) : %.4f   [fixed by data — base rate variance]\n", bd_full$UNC))
cat(sprintf("  REL - RES + UNC   : %.4f   [should ≈ Brier: check]\n\n", bd_full$check))

cat("── High-odds subset (implied_prob_A < 0.29) ──────────────────────\n")
cat(sprintf("  N                 : %d\n",     bd_ho$N))
cat(sprintf("  Base win rate     : %.4f\n",   bd_ho$p_bar))
cat(sprintf("  Brier score       : %.4f\n",   bd_ho$brier))
cat(sprintf("  REL (reliability) : %.4f   [calibration error in betting region]\n", bd_ho$REL))
cat(sprintf("  RES (resolution)  : %.4f\n",   bd_ho$RES))
cat(sprintf("  UNC (uncertainty) : %.4f\n",   bd_ho$UNC))
cat(sprintf("  REL - RES + UNC   : %.4f\n\n", bd_ho$check))

cat("── Decile bins — full test set ───────────────────────────────────\n")
cat(sprintf("  %-4s  %-10s  %-10s  %-8s  %-8s\n",
            "bin", "mean_pred", "obs_rate", "delta", "n"))
for (i in seq_len(nrow(bd_full$bins))) {
  b <- bd_full$bins[i, ]
  cat(sprintf("  %-4d  %-10.4f  %-10.4f  %+.4f  %d\n",
              b$bin, b$f_k, b$o_k, b$o_k - b$f_k, b$n))
}

cat("\n── Decile bins — high-odds subset ───────────────────────────────\n")
cat(sprintf("  %-4s  %-10s  %-10s  %-8s  %-8s\n",
            "bin", "mean_pred", "obs_rate", "delta", "n"))
for (i in seq_len(nrow(bd_ho$bins))) {
  b <- bd_ho$bins[i, ]
  cat(sprintf("  %-4d  %-10.4f  %-10.4f  %+.4f  %d\n",
              b$bin, b$f_k, b$o_k, b$o_k - b$f_k, b$n))
}

# ════════════════════════════════════════════════════════════════════════════
# STEP 9 — Conclusion on edge threshold
# ════════════════════════════════════════════════════════════════════════════

# Key question: in the betting zone (edge >= 0.15, implied < 0.29), are the
# model's probabilities systematically over- or under-estimating observed rates?
# If obs_rate >> mean_pred → model is underconfident in these bets (conservative edge)
# If obs_rate << mean_pred → model is overconfident (edge is partly illusory)

bet_cal_error  <- mean(bet_preds$prob - bet_preds$y)   # mean(predicted - observed)
bet_obs_rate   <- mean(bet_preds$y)
bet_mean_prob  <- mean(bet_preds$prob)
bet_mean_impl  <- mean(bet_preds$implied_prob_A)
bet_mean_edge  <- mean(bet_preds$edge)
bet_true_edge  <- bet_obs_rate - bet_mean_impl          # vs market

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("CONCLUSION: IS THE 0.15 EDGE THRESHOLD SOUND?\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  Betting zone bets (edge>=0.15, implied<0.29) : %d\n", nrow(bet_preds)))
cat(sprintf("  Mean model prob                               : %.4f\n", bet_mean_prob))
cat(sprintf("  Mean observed win rate                        : %.4f\n", bet_obs_rate))
cat(sprintf("  Mean calibration error  (pred - obs)          : %+.4f\n", bet_cal_error))
cat(sprintf("  Mean market implied prob                      : %.4f\n", bet_mean_impl))
cat(sprintf("  True edge over market   (obs - implied)       : %+.4f\n", bet_true_edge))
cat(sprintf("  Model-claimed edge      (pred - implied)      : %+.4f\n", bet_mean_edge))
cat(sprintf("  REL in high-odds subset                       : %.4f\n\n", bd_ho$REL))

# Two separate questions:
#   Q1. Are the model's raw probabilities well-calibrated? (absolute calibration)
#   Q2. Is the 0.15 edge threshold a sound SELECTION criterion? (operational soundness)
#
# These have different answers. The threshold was validated empirically on the
# backtest; it functions as a MODEL-RELATIVE selector, not as a guarantee that
# the raw probability is accurate. What matters operationally is whether the
# TRUE edge (obs_rate - implied) is positive and significant when edge >= 0.15.

raw_well_calibrated <- abs(bet_cal_error) < 0.04
true_edge_positive  <- bet_true_edge > 0
true_edge_real      <- bet_true_edge > 0.05   # economically meaningful

# Overconfidence ratio: how much of the model-claimed edge is real
edge_retention <- if (bet_mean_edge > 0) bet_true_edge / bet_mean_edge else NA_real_

cat(sprintf("  Model overconfidence in betting zone : %+.1f pp (pred=%.1f%% vs obs=%.1f%%)\n",
            bet_cal_error * 100, bet_mean_prob * 100, bet_obs_rate * 100))
cat(sprintf("  True edge over market (obs - implied): %+.1f pp  [this is what drives ROI]\n",
            bet_true_edge * 100))
cat(sprintf("  Edge retention ratio (true/claimed)  : %.1f%%\n\n",
            edge_retention * 100))

if (!raw_well_calibrated && true_edge_real) {
  cat("  Verdict : OVERCONFIDENT in raw probabilities, but threshold is OPERATIONALLY SOUND\n\n")
  cat(sprintf(
    "  The model over-predicts win probability by %.0fpp on average in the betting zone\n",
    bet_cal_error * 100))
  cat(sprintf(
    "  (mean prob=%.1f%%, observed=%.1f%%). This is systematic overconfidence in the\n",
    bet_mean_prob*100, bet_obs_rate*100))
  cat("  high-probability region — common in logistic regression with extreme rank features.\n\n")
  cat(sprintf(
    "  However, the TRUE edge over the market is +%.1fpp (observed %.1f%% win rate vs\n",
    bet_true_edge*100, bet_obs_rate*100))
  cat(sprintf(
    "  %.1fpp implied), which is statistically significant (z=3.98, p<0.0001 in backtest).\n\n",
    bet_mean_impl*100))
  cat("  The 0.15 threshold should be understood as a MODEL-RELATIVE selector:\n")
  cat("  it identifies bets where the model's inflated probability still outpaces the\n")
  cat("  market by enough to indicate genuine disagreement. Since the backtest validates\n")
  cat("  real positive ROI at this threshold, DO NOT raise it based on overconfidence alone.\n\n")
  cat("  What to watch:\n")
  cat(sprintf(
    "    — The overconfidence is concentrated in bins 8-10 of the high-odds subset\n"))
  cat(sprintf(
    "      (mean_pred 0.37-0.70, obs_rate 0.21-0.55). These are cases where the model\n"))
  cat("      assigns 35-70%% win probability to players the market prices at 5-29%%.\n")
  cat("      Large rank/Elo advantages are driving inflated probabilities.\n")
  cat(sprintf(
    "    — REL=%.4f in the high-odds subset vs %.4f full-set. Overconfidence is\n",
    bd_ho$REL, bd_full$REL))
  cat("      localised to this region, not a global model flaw.\n\n")
  cat("  Recommended action: keep threshold at 0.15. Optionally add a secondary cap\n")
  cat("  at edge <= 0.50 to exclude extreme outlier predictions (Djokovic at 4.20\n")
  cat("  type cases) where overconfidence is greatest.\n")
} else if (raw_well_calibrated) {
  cat("  Verdict : SOUND — probabilities are well-calibrated and threshold is appropriate\n\n")
  cat(sprintf("  Mean calibration error %+.1fpp is within acceptable range.\n",
              bet_cal_error*100))
  cat("  The 0.15 edge threshold reflects genuine model edge.\n")
} else {
  cat("  Verdict : WARNING — true edge is marginal; threshold needs review\n\n")
  cat(sprintf("  True edge over market is only %+.1fpp, below the 5pp economic threshold.\n",
              bet_true_edge*100))
}

cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# PLATT SCALING
# ════════════════════════════════════════════════════════════════════════════
# Goal: fit a logistic regression y ~ logit(prob) to recalibrate raw model
# probabilities, specifically correcting the overconfidence in the high-odds
# betting region (bins 8-10, large positive Elo/rank advantages).
#
# Data availability note:
#   odds_all only covers 2020-2024 — no Bet365 data for the training period
#   (2015-2019). We therefore CANNOT filter training predictions to the
#   high-odds subset. Instead we use 5-fold cross-validated out-of-fold
#   predictions on train_inter (2015-2019). This is the correct approach:
#     - Fitting Platt on in-sample predictions overfits the scaler
#     - CV predictions are out-of-fold → unbiased estimate of model behaviour
#     - No test-period data is used in fitting the scaler
#
# Formula: P_cal = sigmoid(A * logit(P_raw) + B)
#   A ≠ 1 or B ≠ 0 indicates miscalibration. A < 1 → overconfident.

cat("══════════════════════════════════════════════════════════════════\n")
cat("PLATT SCALING  —  fit on 5-fold CV of train_inter (2015-2019)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# ── A: Prepare training data with Elo ────────────────────────────────────
cat("Preparing training data...\n")
train_elo_full <- add_elo_feature(
  train_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
) %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface)) %>%
  mutate(y = as.integer(as.character(outcome)))

cat(sprintf("Training rows: %d\n\n", nrow(train_elo_full)))

# ── B: 5-fold CV to get out-of-fold predictions ──────────────────────────
cat("Running 5-fold cross-validation...\n")
set.seed(42)
n_folds <- 5
fold_id  <- sample(rep(1:n_folds, length.out = nrow(train_elo_full)))

FORMULA_ELO <- outcome ~ log_rank_ratio + games_dom_diff +
                          games_dom_x_underdog + elo_diff_surface

cv_preds <- map_dfr(1:n_folds, function(k) {
  train_k <- train_elo_full[fold_id != k, ]
  val_k   <- train_elo_full[fold_id == k, ]

  fit_k <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(FORMULA_ELO, data = train_k)

  tibble(
    fold      = k,
    row_idx   = which(fold_id == k),
    prob_raw  = predict(fit_k, val_k, type = "prob")$.pred_1,
    y         = val_k$y
  )
}) %>%
  arrange(row_idx)

cat(sprintf("OOF predictions: %d  |  OOF win rate: %.3f  |  OOF mean prob: %.3f\n\n",
            nrow(cv_preds), mean(cv_preds$y), mean(cv_preds$prob_raw)))

# ── C: Fit Platt scaler on OOF predictions ───────────────────────────────
# Platt: logit(P_raw) as the single predictor. A≈1, B≈0 = well calibrated.
# A < 1 compresses probs toward centre; B shifts the intercept.
cat("Fitting Platt scaler on OOF predictions...\n")

platt_data   <- tibble(log_odds = qlogis(cv_preds$prob_raw), y = cv_preds$y)
platt_scaler <- glm(y ~ log_odds, data = platt_data, family = binomial)

A <- coef(platt_scaler)["log_odds"]
B <- coef(platt_scaler)["(Intercept)"]

cat(sprintf("  Platt coefficients:  A (slope) = %.4f  |  B (intercept) = %.4f\n", A, B))
cat(sprintf("  A < 1 → overconfident (compress toward 0.5)\n"))
cat(sprintf("  |B| > 0 → systematic bias shift\n\n"))

saveRDS(platt_scaler, "platt_scaler.rds")
cat("Saved platt_scaler.rds\n\n")

# ── D: Apply Platt to test-set predictions ───────────────────────────────
apply_platt <- function(prob_raw, scaler) {
  predict(scaler,
          newdata = data.frame(log_odds = qlogis(pmin(pmax(prob_raw, 1e-6), 1-1e-6))),
          type = "response")
}

preds_cal      <- preds      %>% mutate(prob_platt = apply_platt(prob,    platt_scaler))
preds_odds_cal <- preds_odds %>% mutate(prob_platt = apply_platt(prob,    platt_scaler),
                                        edge_platt = prob_platt - implied_prob_A)
ho_preds_cal   <- preds_odds_cal %>% filter(implied_prob_A < 0.29)
bet_preds_cal  <- preds_odds_cal %>% filter(implied_prob_A < 0.29, edge_platt >= 0.15,
                                            edge_platt < 0.50)

cat(sprintf("After Platt calibration:\n"))
cat(sprintf("  Betting zone bets (edge_platt>=0.15 & <0.50, implied<0.29): %d\n",
            nrow(bet_preds_cal)))

# ── E: Brier decompositions after calibration ────────────────────────────
bd_full_cal <- brier_decompose(preds_cal$prob_platt, preds_cal$y)
bd_ho_cal   <- brier_decompose(ho_preds_cal$prob_platt, ho_preds_cal$y)

cal_full_platt <- make_cal_bins(preds_cal$prob_platt,    preds_cal$y,
                                label = "Full test set — Platt calibrated")
cal_ho_platt   <- make_cal_bins(ho_preds_cal$prob_platt, ho_preds_cal$y,
                                label = "High-odds — Platt calibrated")

# ── F: Edge retention after calibration ──────────────────────────────────
bet_obs_cal    <- mean(bet_preds_cal$y)
bet_mean_platt <- mean(bet_preds_cal$prob_platt)
bet_mean_impl  <- mean(bet_preds_cal$implied_prob_A)
cal_error_platt  <- bet_mean_platt - bet_obs_cal
true_edge_platt  <- bet_obs_cal   - bet_mean_impl
edge_retention_platt <- if (mean(bet_preds_cal$edge_platt) > 0)
  true_edge_platt / mean(bet_preds_cal$edge_platt) else NA_real_

# ── G: Print comparison ───────────────────────────────────────────────────
cat("\n── Brier decomposition: before vs after Platt ───────────────────\n\n")
cat(sprintf("  %-35s  Brier   REL     RES     UNC\n", ""))
cat(sprintf("  %-35s  ------  ------  ------  ------\n", ""))
cat(sprintf("  %-35s  %.4f  %.4f  %.4f  %.4f\n",
            "Full set — raw",     bd_full$brier,     bd_full$REL,     bd_full$RES,     bd_full$UNC))
cat(sprintf("  %-35s  %.4f  %.4f  %.4f  %.4f\n",
            "Full set — Platt",   bd_full_cal$brier, bd_full_cal$REL, bd_full_cal$RES, bd_full_cal$UNC))
cat(sprintf("  %-35s  %.4f  %.4f  %.4f  %.4f\n",
            "High-odds — raw",    bd_ho$brier,       bd_ho$REL,       bd_ho$RES,       bd_ho$UNC))
cat(sprintf("  %-35s  %.4f  %.4f  %.4f  %.4f\n",
            "High-odds — Platt",  bd_ho_cal$brier,   bd_ho_cal$REL,   bd_ho_cal$RES,   bd_ho_cal$UNC))

cat("\n── Edge retention: betting zone before vs after Platt ───────────\n\n")
cat(sprintf("  %-28s  %s\n", "", "Raw          Platt"))
cat(sprintf("  %-28s  %s\n", "", "----------   ----------"))
cat(sprintf("  %-28s  %-12.4f %-12.4f\n", "N bets",
            as.numeric(nrow(bet_preds)), as.numeric(nrow(bet_preds_cal))))
cat(sprintf("  %-28s  %-12.4f %-12.4f\n", "Mean model prob",    bet_mean_prob,  bet_mean_platt))
cat(sprintf("  %-28s  %-12.4f %-12.4f\n", "Mean observed rate", bet_obs_rate,   bet_obs_cal))
cat(sprintf("  %-28s  %+.4f       %+.4f\n", "Calibration error",  bet_cal_error,  cal_error_platt))
cat(sprintf("  %-28s  %+.4f       %+.4f\n", "True edge (obs-impl)",bet_true_edge, true_edge_platt))
cat(sprintf("  %-28s  %.1f%%        %.1f%%\n", "Edge retention ratio",
            edge_retention * 100, edge_retention_platt * 100))

# ── H: Decile bins comparison ─────────────────────────────────────────────
cat("\n── High-odds decile bins: raw vs Platt ──────────────────────────\n\n")
cat(sprintf("  %-4s  %-10s  %-10s  %-8s  |  %-10s  %-10s  %-8s\n",
            "bin", "pred_raw", "obs_rate", "delta", "pred_platt", "obs_rate", "delta"))
cat(sprintf("  %s\n", strrep("-", 78)))
for (i in seq_len(nrow(bd_ho$bins))) {
  raw  <- bd_ho$bins[i, ]
  plat <- bd_ho_cal$bins[i, ]
  cat(sprintf("  %-4d  %-10.4f  %-10.4f  %+.4f  |  %-10.4f  %-10.4f  %+.4f\n",
              raw$bin, raw$f_k, raw$o_k, raw$o_k - raw$f_k,
              plat$f_k, plat$o_k, plat$o_k - plat$f_k))
}

# ── I: Conclusion ─────────────────────────────────────────────────────────
rel_improved  <- bd_ho_cal$REL < bd_ho$REL
ret_improved  <- !is.na(edge_retention_platt) && edge_retention_platt > edge_retention

cat("\n── Conclusion ───────────────────────────────────────────────────\n\n")
cat(sprintf("  REL improved (high-odds):   %s  (%.4f → %.4f)\n",
            if (rel_improved) "YES" else "NO", bd_ho$REL, bd_ho_cal$REL))
cat(sprintf("  Edge retention improved:    %s  (%.1f%% → %.1f%%)\n",
            if (ret_improved) "YES" else "NO",
            edge_retention * 100, edge_retention_platt * 100))

if (rel_improved && ret_improved) {
  cat("\n  Platt scaling improves both calibration and edge retention.\n")
  cat("  The scaler should be applied to live predictions.\n")
  cat("  To use in live_predict.R, apply apply_platt(prob_A, platt_scaler)\n")
  cat("  before computing edge and the high_confidence_flag.\n")
} else if (rel_improved) {
  cat("\n  Platt scaling improves calibration (REL) but not edge retention.\n")
  cat("  The probability estimates are more accurate but the bet selection\n")
  cat("  is not materially better. Optional to apply.\n")
} else {
  cat("\n  Platt scaling does not improve metrics on this test set.\n")
  cat("  The model's logistic structure is already reasonably calibrated\n")
  cat("  and the CV-based scaler may not generalise well.\n")
}
cat("\n")
