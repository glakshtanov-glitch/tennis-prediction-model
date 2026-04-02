# retrain_and_validate.R
# Clean retrain of model_inter3_elo on 2015-2019, test on 2020-2024.
# Runs full backtest + Poisson-binomial significance tests in one pass.
# Compares fresh retrained model to the currently saved model_inter3_elo.rds.

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

elo_lookup         <- readRDS("elo_surface_lookup.rds")
model_saved        <- readRDS("model_inter3_elo.rds")

# ════════════════════════════════════════════════════════════════════════════
# STEP 1 — Confirm train/test date ranges
# ════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════\n")
cat("TRAIN / TEST SPLIT CONFIRMATION\n")
cat("══════════════════════════════════════════════════════\n")

train_years <- range(as.integer(substr(as.character(train_inter$tourney_date), 1, 4)))
test_years  <- range(as.integer(substr(as.character(test_inter$tourney_date),  1, 4)))

cat(sprintf("train_inter: %d rows  |  years %d – %d\n",
            nrow(train_inter), train_years[1], train_years[2]))
cat(sprintf("test_inter:  %d rows  |  years %d – %d\n\n",
            nrow(test_inter),  test_years[1],  test_years[2]))

# ════════════════════════════════════════════════════════════════════════════
# STEP 2 — Add Elo to train and test
# ════════════════════════════════════════════════════════════════════════════

cat("Adding elo_diff_surface...\n")

train_elo <- add_elo_feature(
  train_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
)
test_elo <- add_elo_feature(
  test_inter %>% mutate(outcome = factor(outcome, levels = c("0","1"))),
  elo_lookup
)

train_clean <- train_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))
test_clean <- test_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))

cat(sprintf("train_clean: %d rows  |  test_clean: %d rows\n\n",
            nrow(train_clean), nrow(test_clean)))

# ════════════════════════════════════════════════════════════════════════════
# STEP 3 — Retrain model_inter3_elo from scratch
# ════════════════════════════════════════════════════════════════════════════

cat("Retraining model_inter3_elo on 2015-2019...\n")

model_retrained <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface,
      data = train_clean)

cat("\n── Coefficients: retrained vs saved ────────────────────\n")
coef_new   <- coef(model_retrained$fit)
coef_saved <- coef(model_saved$fit)
coef_compare <- data.frame(
  term      = names(coef_new),
  retrained = round(coef_new,   6),
  saved     = round(coef_saved, 6),
  delta     = round(coef_new - coef_saved, 8)
)
print(coef_compare, row.names = FALSE)

# ════════════════════════════════════════════════════════════════════════════
# STEP 4 — Accuracy / AUC / Brier
# ════════════════════════════════════════════════════════════════════════════

eval_model <- function(model, test_df, label) {
  p <- predict(model, test_df, type = "prob")$.pred_1
  y <- as.integer(as.character(test_df$outcome))
  acc   <- mean((p >= 0.5) == (y == 1))
  brier <- mean((p - y)^2)
  r     <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  auc   <- (sum(r[y == 1]) - n1*(n1+1)/2) / (n1 * n0)
  tibble(model = label, n = nrow(test_df), accuracy = round(acc,4),
         brier = round(brier,4), auc = round(auc,4))
}

cat("\n── Predictive accuracy on test set (2020-2024) ─────────\n")
metrics <- bind_rows(
  eval_model(model_retrained, test_clean, "retrained"),
  eval_model(model_saved,     test_clean, "saved (from .rds)")
)
print(metrics, n = 5)

# ════════════════════════════════════════════════════════════════════════════
# STEP 5 — Backtest: odds join + high-odds subset
# ════════════════════════════════════════════════════════════════════════════

cat("\nBuilding odds join...\n")

odds_prep <- odds_all %>%
  rename(surface_odds = Surface) %>%
  filter(!is.na(B365W), !is.na(B365L)) %>%
  mutate(
    date_parts = str_split(Date, "/"),
    odds_month = map_chr(date_parts, ~ sprintf("%02d", as.integer(.x[1]))),
    odds_year  = map_chr(date_parts, ~ .x[3]),
    date_key   = paste0(odds_year, "-", odds_month),
    overround  = 1/B365W + 1/B365L,
    norm_prob_W = (1/B365W) / overround,
    norm_prob_L = (1/B365L) / overround
  ) %>%
  select(winner_abbrev = Winner, loser_abbrev = Loser,
         surface_odds, date_key, B365W, B365L, norm_prob_W, norm_prob_L)

join_and_dedup <- function(feat_df, model, label) {
  p <- predict(model, feat_df, type = "prob")$.pred_1
  df <- feat_df %>% mutate(prob = p)

  prep <- df %>%
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
           won_bet     = outcome_num == 1,
           edge        = prob - implied_prob_A,
           model_label = label) %>%
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

preds_retrained <- join_and_dedup(test_clean, model_retrained, "retrained")
preds_saved     <- join_and_dedup(test_clean, model_saved,     "saved")

ho_retrained <- preds_retrained %>% filter(implied_prob_A < 0.29)
ho_saved     <- preds_saved     %>% filter(implied_prob_A < 0.29)

cat(sprintf("High-odds rows matched — retrained: %d  |  saved: %d\n\n",
            nrow(ho_retrained), nrow(ho_saved)))

# ════════════════════════════════════════════════════════════════════════════
# STEP 6 — Sweep thresholds
# ════════════════════════════════════════════════════════════════════════════

sweep <- function(df, label, thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25)) {
  map_dfr(thresholds, function(thr) {
    bets <- df %>% filter(edge >= thr)
    n    <- nrow(bets)
    if (n == 0) return(tibble(model=label, threshold=thr, n_bets=0L,
                              win_rate=NA_real_, roi=NA_real_,
                              avg_odds=NA_real_, avg_edge=NA_real_))
    tibble(model     = label,
           threshold = thr,
           n_bets    = n,
           win_rate  = mean(bets$won_bet),
           roi       = sum(if_else(bets$won_bet, bets$odds_A - 1, -1)) / n,
           avg_odds  = mean(bets$odds_A),
           avg_edge  = mean(bets$edge))
  })
}

backtest <- bind_rows(
  sweep(ho_retrained, "retrained"),
  sweep(ho_saved,     "saved")
) %>% mutate(across(where(is.numeric) & !c(threshold, n_bets), ~ round(., 4)))

cat("══════════════════════════════════════════════════════════════════\n")
cat("BACKTEST  |  HIGH-ODDS (implied < 0.29)  |  2020-2024  |  flat staking\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
print(backtest %>% arrange(threshold, model), n = 20)

# ════════════════════════════════════════════════════════════════════════════
# STEP 7 — Poisson-binomial significance tests
# ════════════════════════════════════════════════════════════════════════════

run_significance <- function(bets_df, threshold, label) {
  bets <- bets_df %>% filter(edge >= threshold)
  n    <- nrow(bets)
  if (n == 0) return(NULL)

  wins  <- sum(bets$won_bet)
  wr    <- wins / n
  ps    <- bets$implied_prob_A
  p_avg <- mean(ps)

  # Poisson-binomial normal approximation
  exp_w <- sum(ps)
  sd_w  <- sqrt(sum(ps * (1 - ps)))
  z     <- (wins - exp_w) / sd_w
  p_pb  <- pnorm(z, lower.tail = FALSE)

  # Wilson CI
  z95    <- qnorm(0.975)
  denom  <- 1 + z95^2 / n
  centre <- (wr + z95^2 / (2*n)) / denom
  margin <- z95 * sqrt(wr*(1-wr)/n + z95^2/(4*n^2)) / denom

  tibble(
    model          = label,
    threshold      = threshold,
    n_bets         = n,
    wins           = wins,
    win_rate       = round(wr,    4),
    avg_implied    = round(p_avg, 4),
    edge_over_mkt  = round(wr - p_avg, 4),
    z_score        = round(z,    3),
    p_value        = round(p_pb, 5),
    ci_lo          = round(centre - margin, 4),
    ci_hi          = round(centre + margin, 4),
    significant    = p_pb < 0.05
  )
}

thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25)

sig_results <- bind_rows(
  map_dfr(thresholds, ~ run_significance(ho_retrained, .x, "retrained")),
  map_dfr(thresholds, ~ run_significance(ho_saved,     .x, "saved"))
) %>% arrange(threshold, model)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("SIGNIFICANCE TESTS  |  H0: win_rate = implied_prob  |  one-sided\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
print(sig_results %>%
        select(model, threshold, n_bets, win_rate, avg_implied, edge_over_mkt,
               z_score, p_value, significant),
      n = 20)

cat("\n── Wilson 95% CIs on win rate ───────────────────────────────────\n\n")
print(sig_results %>%
        select(model, threshold, n_bets, win_rate, avg_implied, ci_lo, ci_hi) %>%
        mutate(implied_in_ci = avg_implied >= ci_lo & avg_implied <= ci_hi),
      n = 20)

# ════════════════════════════════════════════════════════════════════════════
# STEP 8 — Save retrained model (replaces saved .rds if coefficients differ)
# ════════════════════════════════════════════════════════════════════════════

max_delta <- max(abs(coef_compare$delta))
if (max_delta < 1e-6) {
  cat("\n[Coefficients identical to saved model — no file update needed]\n")
} else {
  cat(sprintf("\n[Max coefficient delta = %.2e — saving retrained model]\n", max_delta))
  saveRDS(model_retrained, "model_inter3_elo.rds")
  cat("Saved model_inter3_elo.rds\n")
}

cat("\nDone.\n")
