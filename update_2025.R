# update_2025.R
# Integrates 2025 ATP season data (tennis-data.co.uk CSV):
#   Phase A — Name reverse lookup (abbreviated → full Sackmann name)
#   Phase B — Parse 2025 CSV into Sackmann-compatible rows
#   Phase C — Extend df_with_surface, recompute elo_surface_lookup.rds
#   Phase D — Build 2025 feature table (both orientations per match)
#   Phase E — Verify model unchanged (coef delta < 1e-6)
#   Phase F — Run 2025-only backtest, sweep thresholds, significance test
#
# Outputs: elo_surface_lookup.rds (extended), 2025 backtest results to console.
# Model coefficients are unchanged (training data 2015-2019 is identical).

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

model_saved    <- readRDS("model_inter3_elo.rds")
elo_lookup_old <- readRDS("elo_surface_lookup.rds")

CSV_PATH <- "C:/Users/User/OneDrive/Documents/tennis_model/2025_2025_.csv.csv"

cat("── Loading 2025 CSV ─────────────────────────────────────────────────\n")
csv_2025 <- read_csv(CSV_PATH, show_col_types = FALSE)
cat(sprintf("  Rows: %d  |  Cols: %d\n\n", nrow(csv_2025), ncol(csv_2025)))

# ── Phase A: Name reverse lookup ──────────────────────────────────────────────
cat("── Phase A: Building abbreviated → full name lookup ────────────────\n")

all_players <- bind_rows(
  df_with_surface %>% distinct(player = winner_name),
  df_with_surface %>% distinct(player = loser_name)
) %>% distinct()

name_map <- all_players %>%
  mutate(abbrev = map_chr(player, abbreviate_name))

# On collision keep first occurrence (most common player for that abbreviation)
abbrev_to_full <- name_map %>%
  group_by(abbrev) %>%
  slice(1) %>%
  ungroup() %>%
  select(abbrev, player) %>%
  deframe()   # named vector: abbrev_to_full["Sinner J."] = "Jannik Sinner"

cat(sprintf("  Historical players: %d  |  abbreviations mapped: %d\n\n",
            nrow(all_players), length(abbrev_to_full)))

# ── Phase B: Parse 2025 CSV ───────────────────────────────────────────────────
cat("── Phase B: Parsing 2025 CSV ────────────────────────────────────────\n")

round_map <- c(
  "1st Round"      = 1L,
  "2nd Round"      = 2L,
  "3rd Round"      = 3L,
  "4th Round"      = 4L,
  "Quarter-Finals" = 5L,
  "Semi-Finals"    = 6L,
  "Finals"         = 7L,
  "Round Robin"    = 1L
)

max_hist_id <- max(df_with_surface$match_id)

matches_2025 <- csv_2025 %>%
  filter(Comment == "Completed",
         !is.na(B365W), !is.na(B365L),
         !is.na(WRank), !is.na(LRank)) %>%
  mutate(
    # Date: MM/DD/YYYY → YYYYMMDD integer
    date_parts   = str_split(Date, "/"),
    tourney_date = as.integer(paste0(
      map_chr(date_parts, ~ .x[3]),
      sprintf("%02d", as.integer(map_chr(date_parts, ~ .x[1]))),
      sprintf("%02d", as.integer(map_chr(date_parts, ~ .x[2])))
    )),
    match_num    = round_map[Round],
    winner_name  = map_chr(Winner, ~ {
      full <- abbrev_to_full[.x]
      if (is.null(full) || is.na(full)) .x else full
    }),
    loser_name   = map_chr(Loser, ~ {
      full <- abbrev_to_full[.x]
      if (is.null(full) || is.na(full)) .x else full
    }),
    winner_rank  = as.integer(WRank),
    loser_rank   = as.integer(LRank),
    surface      = Surface,
    # Per-match game counts from set scores
    w_games = rowSums(select(., W1, W2, W3, W4, W5), na.rm = TRUE),
    l_games = rowSums(select(., L1, L2, L3, L4, L5), na.rm = TRUE),
    match_id = max_hist_id + row_number()
  ) %>%
  select(match_id, tourney_date, match_num, tournament = Tournament,
         surface, winner_name, loser_name, winner_rank, loser_rank,
         w_games, l_games, Winner, Loser, B365W, B365L)

cat(sprintf("  Parsed: %d matches  |  match_id range: %d – %d\n\n",
            nrow(matches_2025), min(matches_2025$match_id), max(matches_2025$match_id)))

# Spot-check name resolution
n_resolved_w <- sum(matches_2025$winner_name != matches_2025$Winner)
n_resolved_l <- sum(matches_2025$loser_name  != matches_2025$Loser)
cat(sprintf("  Name resolution: %d winners, %d losers mapped to full names\n\n",
            n_resolved_w, n_resolved_l))

# ── Phase C: Extend df_with_surface and recompute Elo ─────────────────────────
cat("── Phase C: Extending Elo lookup through 2025 ───────────────────────\n")

df_extended <- bind_rows(
  df_with_surface %>%
    select(match_id, tourney_date, match_num, surface,
           winner_name, loser_name, winner_rank, loser_rank),
  matches_2025 %>%
    select(match_id, tourney_date, match_num, surface,
           winner_name, loser_name, winner_rank, loser_rank)
)

# Rebuild rank_init_df: replicate exactly how elo_init_experiment.R built it for
# historical data (sorted by tourney_date + match_num, bind winner then loser),
# then extend with any genuinely new players first seen in 2025.

sorted_hist <- df_with_surface %>% arrange(tourney_date, match_num)
rank_init_hist <- bind_rows(
  sorted_hist %>% transmute(player = winner_name, surface, rank = winner_rank),
  sorted_hist %>% transmute(player = loser_name,  surface, rank = loser_rank)
) %>%
  filter(!is.na(rank)) %>%
  group_by(player, surface) %>%
  slice(1) %>%
  ungroup()

sorted_2025 <- matches_2025 %>% arrange(tourney_date, match_num)
rank_init_2025_raw <- bind_rows(
  sorted_2025 %>% transmute(player = winner_name, surface, rank = winner_rank),
  sorted_2025 %>% transmute(player = loser_name,  surface, rank = loser_rank)
) %>%
  filter(!is.na(rank)) %>%
  group_by(player, surface) %>%
  slice(1) %>%
  ungroup()

existing_keys <- paste(rank_init_hist$player, rank_init_hist$surface)
rank_init_df <- bind_rows(
  rank_init_hist,
  rank_init_2025_raw %>% filter(!paste(player, surface) %in% existing_keys)
)

elo_lookup_new <- compute_surface_elo(df_extended, K = 32, rank_init_df = rank_init_df)
cat(sprintf("  Old lookup: %d rows  |  New lookup: %d rows  |  Added: %d\n",
            nrow(elo_lookup_old), nrow(elo_lookup_new),
            nrow(elo_lookup_new) - nrow(elo_lookup_old)))

# Verify historical Elo values unchanged
old_ids  <- elo_lookup_old$match_id
new_hist <- elo_lookup_new %>% filter(match_id %in% old_ids) %>% arrange(match_id)
old_ord  <- elo_lookup_old %>% arrange(match_id)

max_drift_w <- max(abs(new_hist$elo_winner_pre - old_ord$elo_winner_pre))
max_drift_l <- max(abs(new_hist$elo_loser_pre  - old_ord$elo_loser_pre))
cat(sprintf("  Elo drift in historical rows: winner=%.2e  loser=%.2e\n\n",
            max_drift_w, max_drift_l))

if (max(max_drift_w, max_drift_l) > 1e-6) {
  warning("Unexpected Elo drift in historical rows — check rank_init_df construction.")
}

saveRDS(elo_lookup_new, "elo_surface_lookup.rds")
cat("  Saved elo_surface_lookup.rds\n\n")

# ── Phase D: Build 2025 feature table (both orientations) ─────────────────────
cat("── Phase D: Building 2025 feature table ─────────────────────────────\n")

# Compute within-tournament cumulative games_dom for each player from prior rounds.
# Sort matches_2025 by (tourney_date, match_num) within each tournament.
# For each match row, games_dom = prior-rounds cumulative wins / (wins+losses).
# First match in a tournament = 0.5 (neutral).

matches_sorted <- matches_2025 %>%
  arrange(tourney_date, tournament, match_num, match_id)

compute_games_dom_col <- function(player_col, w_games_col, l_games_col,
                                   is_winner_col, tourn_col, match_id_col) {
  n <- length(player_col)
  games_dom <- numeric(n)

  for (i in seq_len(n)) {
    tourn   <- tourn_col[i]
    mid     <- match_id_col[i]
    player  <- player_col[i]

    # Prior rows in same tournament (strictly before current match_id)
    prior_mask <- tourn_col == tourn & match_id_col < mid
    prior_idx  <- which(prior_mask)

    if (length(prior_idx) == 0) {
      games_dom[i] <- 0.5
      next
    }

    won_games  <- 0
    lost_games <- 0
    for (j in prior_idx) {
      if (player == player_col[j] || is_winner_col[j] == (player == player_col[j])) {
        # Check if player was winner or loser in match j
        if (is_winner_col[j]) {
          won_games  <- won_games  + w_games_col[j]
          lost_games <- lost_games + l_games_col[j]
        } else {
          won_games  <- won_games  + l_games_col[j]
          lost_games <- lost_games + w_games_col[j]
        }
      }
    }

    total <- won_games + lost_games
    games_dom[i] <- if (total == 0) 0.5 else won_games / total
  }
  games_dom
}

# Build a lookup table: for each match_id × player, their prior games_dom
# More efficient: build cumulative stats per (tournament, player) iterating in order
build_games_dom_lookup <- function(ms) {
  # ms must be pre-sorted by tourney_date, tournament, match_num, match_id
  # Returns ms with columns games_dom_winner, games_dom_loser added
  n <- nrow(ms)
  gd_winner <- numeric(n)
  gd_loser  <- numeric(n)

  # Running totals per (tournament, player): list keyed by "tournament::player"
  running <- list()

  for (i in seq_len(n)) {
    tourn  <- ms$tournament[i]
    winner <- ms$winner_name[i]
    loser  <- ms$loser_name[i]
    wg     <- ms$w_games[i]
    lg     <- ms$l_games[i]

    key_w <- paste0(tourn, "::", winner)
    key_l <- paste0(tourn, "::", loser)

    # Pre-match running totals for winner
    rw <- running[[key_w]]
    if (is.null(rw)) {
      gd_winner[i] <- 0.5
    } else {
      total <- rw[1] + rw[2]
      gd_winner[i] <- if (total == 0) 0.5 else rw[1] / total
    }

    # Pre-match running totals for loser
    rl <- running[[key_l]]
    if (is.null(rl)) {
      gd_loser[i] <- 0.5
    } else {
      total <- rl[1] + rl[2]
      gd_loser[i] <- if (total == 0) 0.5 else rl[1] / total
    }

    # Update running totals after match
    running[[key_w]] <- c((if (is.null(rw)) 0 else rw[1]) + wg,
                          (if (is.null(rw)) 0 else rw[2]) + lg)
    running[[key_l]] <- c((if (is.null(rl)) 0 else rl[1]) + lg,
                          (if (is.null(rl)) 0 else rl[2]) + wg)
  }

  ms %>% mutate(games_dom_winner = gd_winner, games_dom_loser = gd_loser)
}

ms_with_gd <- build_games_dom_lookup(matches_sorted)

# Orientation A: playerA = Winner, outcome = 1
orient_A <- ms_with_gd %>%
  transmute(
    match_id             = match_id,
    tourney_date         = tourney_date,
    tournament           = tournament,
    surface              = surface,
    playerA              = winner_name,
    playerB              = loser_name,
    rankA                = winner_rank,
    rankB                = loser_rank,
    outcome              = factor("1", levels = c("0", "1")),
    log_rank_ratio       = log(loser_rank / winner_rank),
    games_dom_diff       = games_dom_winner - games_dom_loser,
    games_dom_x_underdog = games_dom_winner * as.numeric(winner_rank > loser_rank),
    winner_abbrev        = Winner,
    loser_abbrev         = Loser
  )

# Orientation B: playerA = Loser, outcome = 0
orient_B <- ms_with_gd %>%
  transmute(
    match_id             = match_id,
    tourney_date         = tourney_date,
    tournament           = tournament,
    surface              = surface,
    playerA              = loser_name,
    playerB              = winner_name,
    rankA                = loser_rank,
    rankB                = winner_rank,
    outcome              = factor("0", levels = c("0", "1")),
    log_rank_ratio       = log(winner_rank / loser_rank),
    games_dom_diff       = games_dom_loser - games_dom_winner,
    games_dom_x_underdog = games_dom_loser * as.numeric(loser_rank > winner_rank),
    winner_abbrev        = Winner,
    loser_abbrev         = Loser
  )

feat_2025_raw <- bind_rows(orient_A, orient_B)

# Add elo_diff_surface using updated lookup
feat_2025 <- feat_2025_raw %>%
  add_elo_feature(elo_lookup_new) %>%
  filter(!is.na(log_rank_ratio), !is.na(elo_diff_surface),
         !is.infinite(log_rank_ratio))

cat(sprintf("  Feature rows (both orientations): %d → %d after NA filter\n\n",
            nrow(feat_2025_raw), nrow(feat_2025)))

# ── Phase E: Verify model unchanged ───────────────────────────────────────────
cat("── Phase E: Verifying model coefficients unchanged ──────────────────\n")

train_verify <- add_elo_feature(
  train_inter %>% mutate(outcome = factor(outcome, levels = c("0", "1"))),
  elo_lookup_new   # 2015-2019 Elo values are identical in new lookup
) %>% filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
             !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))

model_verify <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface,
      data = train_verify)

coef_saved  <- coef(model_saved$fit)
coef_verify <- coef(model_verify$fit)
max_coef_delta <- max(abs(coef_verify - coef_saved))
cat(sprintf("  Max coefficient delta: %.2e  (expect < 1e-6)\n\n", max_coef_delta))

if (max_coef_delta > 1e-4) {
  warning("Large coefficient delta — check whether 2015-2019 Elo values changed.")
}

# ── Phase F: 2025-only backtest ────────────────────────────────────────────────
cat("── Phase F: 2025 backtest ────────────────────────────────────────────\n")

# Build 2025 odds lookup (same structure as retrain_and_validate.R odds_prep)
# Using abbreviated Winner/Loser from the CSV (already in Bet365 format)
odds_2025_prep <- ms_with_gd %>%
  filter(!is.na(B365W), !is.na(B365L)) %>%
  mutate(
    date_parts = str_split(
      sprintf("%s/%s/%s",
              substr(as.character(tourney_date), 5, 6),  # MM
              substr(as.character(tourney_date), 7, 8),  # DD
              substr(as.character(tourney_date), 1, 4)), # YYYY
      "/"
    ),
    odds_month = map_chr(date_parts, ~ sprintf("%02d", as.integer(.x[1]))),
    odds_year  = map_chr(date_parts, ~ .x[3]),
    date_key   = paste0(odds_year, "-", odds_month),
    overround   = 1 / B365W + 1 / B365L,
    norm_prob_W = (1 / B365W) / overround,
    norm_prob_L = (1 / B365L) / overround,
    surface_odds = surface
  ) %>%
  select(winner_abbrev = Winner, loser_abbrev = Loser,
         surface_odds, date_key, B365W, B365L, norm_prob_W, norm_prob_L)

# join_and_dedup for 2025 feature table
# feat_2025 has: match_id, playerA, playerB, rankA, rankB, tourney_date, surface, outcome
# Abbreviation: use winner_abbrev/loser_abbrev stored in feat_2025
join_and_dedup_2025 <- function(feat_df, model) {
  p <- predict(model, feat_df, type = "prob")$.pred_1

  prep <- feat_df %>%
    mutate(
      prob       = p,
      surface_clean = as.character(surface),
      td_str     = as.character(tourney_date),
      date_key   = paste0(substr(td_str, 1, 4), "-", substr(td_str, 5, 6))
    )

  # Both orientations join on the same match key (actual winner/loser from CSV).
  # winner_abbrev and loser_abbrev in feat_2025 are the actual CSV winner/loser,
  # not playerA/playerB. The only difference between orientations is which
  # player is "A" (and therefore which side's odds/implied we extract).

  # Orientation A (outcome=1): playerA = actual winner → bet on winner
  a_wins <- prep %>%
    filter(as.character(outcome) == "1") %>%
    inner_join(odds_2025_prep,
               by = c("winner_abbrev" = "winner_abbrev",
                      "loser_abbrev"  = "loser_abbrev",
                      "surface_clean" = "surface_odds",
                      "date_key"      = "date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365W, implied_prob_A = norm_prob_W)

  # Orientation B (outcome=0): playerA = actual loser → bet on loser
  a_loses <- prep %>%
    filter(as.character(outcome) == "0") %>%
    inner_join(odds_2025_prep,
               by = c("winner_abbrev" = "winner_abbrev",
                      "loser_abbrev"  = "loser_abbrev",
                      "surface_clean" = "surface_odds",
                      "date_key"      = "date_key"),
               relationship = "many-to-many") %>%
    mutate(odds_A = B365L, implied_prob_A = norm_prob_L)

  bind_rows(a_wins, a_loses) %>%
    mutate(
      outcome_num = as.integer(as.character(outcome)),
      won_bet     = outcome_num == 1,
      edge        = prob - implied_prob_A
    ) %>%
    group_by(match_id) %>%
    slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
    ungroup()
}

preds_2025 <- join_and_dedup_2025(feat_2025, model_saved)
ho_2025    <- preds_2025 %>% filter(implied_prob_A < 0.29)

cat(sprintf("  Matched rows: %d  |  High-odds (implied < 0.29): %d\n\n",
            nrow(preds_2025), nrow(ho_2025)))

# Sweep thresholds
sweep <- function(df, label, thresholds = c(0.05, 0.10, 0.15, 0.20, 0.25)) {
  map_dfr(thresholds, function(thr) {
    bets <- df %>% filter(edge >= thr)
    n    <- nrow(bets)
    if (n == 0) return(tibble(threshold = thr, n_bets = 0L,
                              win_rate = NA_real_, roi = NA_real_,
                              avg_odds = NA_real_, avg_edge = NA_real_))
    tibble(
      threshold = thr,
      n_bets    = n,
      win_rate  = round(mean(bets$won_bet), 4),
      roi       = round(sum(if_else(bets$won_bet, bets$odds_A - 1, -1)) / n, 4),
      avg_odds  = round(mean(bets$odds_A), 2),
      avg_edge  = round(mean(bets$edge), 4)
    )
  })
}

# Significance test (Poisson-binomial normal approx)
run_significance <- function(bets_df, threshold) {
  bets <- bets_df %>% filter(edge >= threshold)
  n    <- nrow(bets)
  if (n == 0) return(NULL)
  wins  <- sum(bets$won_bet)
  ps    <- bets$implied_prob_A
  z     <- (wins - sum(ps)) / sqrt(sum(ps * (1 - ps)))
  p_pb  <- pnorm(z, lower.tail = FALSE)
  tibble(threshold   = threshold,
         n_bets      = n,
         wins        = wins,
         win_rate    = round(wins / n, 4),
         avg_implied = round(mean(ps), 4),
         true_edge   = round(wins / n - mean(ps), 4),
         z_score     = round(z, 3),
         p_value     = round(p_pb, 5))
}

backtest_2025 <- sweep(ho_2025, "2025")

cat("══════════════════════════════════════════════════════════════════\n")
cat("2025 BACKTEST  |  implied < 0.29  |  flat staking\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
print(backtest_2025, n = 10)

sig_15 <- run_significance(ho_2025, 0.15)
cat("\n── Significance at edge ≥ 0.15 ──────────────────────────────────────────\n")
if (!is.null(sig_15)) print(sig_15) else cat("  No bets at this threshold.\n")

cat("\n── Significance at all thresholds ───────────────────────────────────────\n")
sig_all <- map_dfr(c(0.05, 0.10, 0.15, 0.20, 0.25), ~ run_significance(ho_2025, .x))
print(sig_all, n = 10)

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════\n")
cat("SUMMARY FOR CLAUDE.md\n")
cat("══════════════════════════════════════════════════════════════════\n")

r15 <- backtest_2025 %>% filter(threshold == 0.15)
s15 <- sig_15

if (!is.null(s15) && nrow(r15) > 0) {
  cat(sprintf(
    "  2025 out-of-sample  |  edge>0.15, implied<0.29\n  n=%d  win_rate=%.1f%%  ROI=%+.1f%%  z=%.2f  p=%.4f\n",
    s15$n_bets, 100*s15$win_rate, 100*r15$roi, s15$z_score, s15$p_value
  ))
} else {
  cat("  No bets at edge > 0.15 in 2025 data.\n")
}

cat("\nDone.\n")
