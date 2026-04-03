# tournament_analysis.R
# Breaks down HC backtest performance (edge > 0.15, implied < 0.29) by:
#   1. Tournament name
#   2. Tournament level (Grand Slam / Masters 1000 / ATP 500 / ATP 250)
#   3. Surface (Hard / Clay / Grass)
#
# Covers 2020–2025: test_inter joined with odds_all (2020–2024) plus the
# 2025 season CSV (2025_2025_.csv.csv).
#
# Output: three tables to console + tournament_roi_breakdown.csv (by name).
# Does NOT modify any existing script or model artefact.

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

model_saved    <- readRDS("model_inter3_elo.rds")
elo_lookup     <- readRDS("elo_surface_lookup.rds")

HC_EDGE    <- 0.15
HC_IMPLIED <- 0.29

# ── Shared summarise helper ────────────────────────────────────────────────────

summarise_bets <- function(df, group_var) {
  df %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      n_bets   = n(),
      wins     = sum(won_bet),
      win_rate = round(wins / n_bets, 3),
      roi      = round(sum(if_else(won_bet, odds_A - 1, -1)) / n_bets, 3),
      avg_odds = round(mean(odds_A), 2),
      avg_edge = round(mean(edge), 3),
      .groups  = "drop"
    ) %>%
    arrange(desc(n_bets))
}

# ════════════════════════════════════════════════════════════════════════════════
# PART A — 2020–2024 via test_inter + odds_all
# ════════════════════════════════════════════════════════════════════════════════

cat("── Building 2020–2024 feature table ─────────────────────────────────\n")

test_elo <- add_elo_feature(
  test_inter %>% mutate(outcome = factor(outcome, levels = c("0", "1"))),
  elo_lookup
)
test_clean <- test_elo %>%
  filter(!is.na(log_rank_ratio), !is.na(games_dom_diff),
         !is.na(games_dom_x_underdog), !is.na(elo_diff_surface))

cat(sprintf("  test_clean: %d rows\n\n", nrow(test_clean)))

# Build odds_prep — carry Tournament and Series for the breakdown
odds_prep <- odds_all %>%
  rename(surface_odds = Surface) %>%
  filter(!is.na(B365W), !is.na(B365L)) %>%
  mutate(
    date_parts  = str_split(Date, "/"),
    odds_month  = map_chr(date_parts, ~ sprintf("%02d", as.integer(.x[1]))),
    odds_year   = map_chr(date_parts, ~ .x[3]),
    date_key    = paste0(odds_year, "-", odds_month),
    overround   = 1 / B365W + 1 / B365L,
    norm_prob_W = (1 / B365W) / overround,
    norm_prob_L = (1 / B365L) / overround
  ) %>%
  select(winner_abbrev = Winner, loser_abbrev = Loser,
         surface_odds, date_key, B365W, B365L, norm_prob_W, norm_prob_L,
         tourn_name = Tournament, series = Series)

p <- predict(model_saved, test_clean, type = "prob")$.pred_1
prep <- test_clean %>%
  mutate(
    prob           = p,
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

preds_2024 <- bind_rows(a_wins, a_loses) %>%
  mutate(
    outcome_num = as.integer(as.character(outcome)),
    won_bet     = outcome_num == 1,
    edge        = prob - implied_prob_A
  ) %>%
  group_by(match_id) %>%
  slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
  ungroup()

ho_2024 <- preds_2024 %>%
  filter(implied_prob_A < HC_IMPLIED, edge >= HC_EDGE) %>%
  select(match_id, won_bet, odds_A, edge, implied_prob_A,
         surface = surface_clean, tourn_name, series)

cat(sprintf("  HC bets 2020–2024: %d\n\n", nrow(ho_2024)))

# ════════════════════════════════════════════════════════════════════════════════
# PART B — 2025 via 2025_2025_.csv.csv
# ════════════════════════════════════════════════════════════════════════════════

cat("── Building 2025 feature table ──────────────────────────────────────\n")

csv_2025 <- read_csv("2025_2025_.csv.csv", show_col_types = FALSE)

# Name reverse lookup (abbreviated → full Sackmann name)
abbrev_to_full <- bind_rows(
  df_with_surface %>% distinct(player = winner_name),
  df_with_surface %>% distinct(player = loser_name)
) %>%
  distinct() %>%
  mutate(abbrev = map_chr(player, abbreviate_name)) %>%
  group_by(abbrev) %>% slice(1) %>% ungroup() %>%
  select(abbrev, player) %>% deframe()

round_map <- c("1st Round" = 1L, "2nd Round" = 2L, "3rd Round" = 3L,
               "4th Round" = 4L, "Quarter-Finals" = 5L,
               "Semi-Finals" = 6L, "Finals" = 7L, "Round Robin" = 1L)

max_hist_id <- max(df_with_surface$match_id)

matches_2025 <- csv_2025 %>%
  filter(Comment == "Completed", !is.na(B365W), !is.na(B365L),
         !is.na(WRank), !is.na(LRank)) %>%
  mutate(
    date_parts   = str_split(Date, "/"),
    tourney_date = as.integer(paste0(
      map_chr(date_parts, ~ .x[3]),
      sprintf("%02d", as.integer(map_chr(date_parts, ~ .x[1]))),
      sprintf("%02d", as.integer(map_chr(date_parts, ~ .x[2])))
    )),
    match_num    = round_map[Round],
    winner_name  = map_chr(Winner, ~ { f <- abbrev_to_full[.x]; if (is.null(f) || is.na(f)) .x else f }),
    loser_name   = map_chr(Loser,  ~ { f <- abbrev_to_full[.x]; if (is.null(f) || is.na(f)) .x else f }),
    winner_rank  = as.integer(WRank),
    loser_rank   = as.integer(LRank),
    surface      = Surface,
    w_games      = rowSums(select(., W1, W2, W3, W4, W5), na.rm = TRUE),
    l_games      = rowSums(select(., L1, L2, L3, L4, L5), na.rm = TRUE),
    match_id     = max_hist_id + row_number()
  ) %>%
  select(match_id, tourney_date, match_num, tournament = Tournament,
         series = Series, surface, winner_name, loser_name,
         winner_rank, loser_rank, w_games, l_games, Winner, Loser, B365W, B365L)

# Within-tournament games_dom
build_games_dom_lookup <- function(ms) {
  ms <- ms %>% arrange(tourney_date, tournament, match_num, match_id)
  n  <- nrow(ms)
  gd_winner <- numeric(n); gd_loser <- numeric(n)
  running <- list()
  for (i in seq_len(n)) {
    key_w <- paste0(ms$tournament[i], "::", ms$winner_name[i])
    key_l <- paste0(ms$tournament[i], "::", ms$loser_name[i])
    rw <- running[[key_w]]; rl <- running[[key_l]]
    gd_winner[i] <- if (is.null(rw)) 0.5 else { tot <- rw[1]+rw[2]; if(tot==0) 0.5 else rw[1]/tot }
    gd_loser[i]  <- if (is.null(rl)) 0.5 else { tot <- rl[1]+rl[2]; if(tot==0) 0.5 else rl[1]/tot }
    running[[key_w]] <- c((if(is.null(rw)) 0 else rw[1]) + ms$w_games[i],
                          (if(is.null(rw)) 0 else rw[2]) + ms$l_games[i])
    running[[key_l]] <- c((if(is.null(rl)) 0 else rl[1]) + ms$l_games[i],
                          (if(is.null(rl)) 0 else rl[2]) + ms$w_games[i])
  }
  ms %>% mutate(games_dom_winner = gd_winner, games_dom_loser = gd_loser)
}

ms_with_gd <- build_games_dom_lookup(matches_2025)

make_orient <- function(ms, as_winner) {
  if (as_winner) {
    ms %>% transmute(
      match_id, tourney_date, surface, tourn_name = tournament, series,
      playerA = winner_name, playerB = loser_name,
      rankA = winner_rank, rankB = loser_rank,
      outcome = factor("1", levels = c("0","1")),
      log_rank_ratio       = log(loser_rank / winner_rank),
      games_dom_diff       = games_dom_winner - games_dom_loser,
      games_dom_x_underdog = games_dom_winner * as.numeric(winner_rank > loser_rank),
      winner_abbrev = Winner, loser_abbrev = Loser
    )
  } else {
    ms %>% transmute(
      match_id, tourney_date, surface, tourn_name = tournament, series,
      playerA = loser_name,  playerB = winner_name,
      rankA = loser_rank,   rankB = winner_rank,
      outcome = factor("0", levels = c("0","1")),
      log_rank_ratio       = log(winner_rank / loser_rank),
      games_dom_diff       = games_dom_loser - games_dom_winner,
      games_dom_x_underdog = games_dom_loser * as.numeric(loser_rank > winner_rank),
      winner_abbrev = Winner, loser_abbrev = Loser
    )
  }
}

feat_2025_raw <- bind_rows(make_orient(ms_with_gd, TRUE),
                            make_orient(ms_with_gd, FALSE))

feat_2025 <- feat_2025_raw %>%
  add_elo_feature(elo_lookup) %>%
  filter(!is.na(log_rank_ratio), !is.na(elo_diff_surface), !is.infinite(log_rank_ratio))

odds_2025_prep <- ms_with_gd %>%
  mutate(
    td_str     = as.character(tourney_date),
    date_key   = paste0(substr(td_str, 1, 4), "-", substr(td_str, 5, 6)),
    overround   = 1 / B365W + 1 / B365L,
    norm_prob_W = (1 / B365W) / overround,
    norm_prob_L = (1 / B365L) / overround,
    surface_odds = surface
  ) %>%
  select(winner_abbrev = Winner, loser_abbrev = Loser,
         surface_odds, date_key, B365W, B365L, norm_prob_W, norm_prob_L)

p25 <- predict(model_saved, feat_2025, type = "prob")$.pred_1
prep25 <- feat_2025 %>%
  mutate(prob = p25,
         surface_clean = as.character(surface),
         td_str = as.character(tourney_date),
         date_key = paste0(substr(td_str, 1, 4), "-", substr(td_str, 5, 6)))

a_wins25 <- prep25 %>%
  filter(as.character(outcome) == "1") %>%
  inner_join(odds_2025_prep,
             by = c("winner_abbrev" = "winner_abbrev", "loser_abbrev" = "loser_abbrev",
                    "surface_clean" = "surface_odds", "date_key" = "date_key"),
             relationship = "many-to-many") %>%
  mutate(odds_A = B365W, implied_prob_A = norm_prob_W)

a_loses25 <- prep25 %>%
  filter(as.character(outcome) == "0") %>%
  inner_join(odds_2025_prep,
             by = c("winner_abbrev" = "winner_abbrev", "loser_abbrev" = "loser_abbrev",
                    "surface_clean" = "surface_odds", "date_key" = "date_key"),
             relationship = "many-to-many") %>%
  mutate(odds_A = B365L, implied_prob_A = norm_prob_L)

preds_2025 <- bind_rows(a_wins25, a_loses25) %>%
  mutate(
    outcome_num = as.integer(as.character(outcome)),
    won_bet     = outcome_num == 1,
    edge        = prob - implied_prob_A
  ) %>%
  group_by(match_id) %>%
  slice_min(implied_prob_A, n = 1, with_ties = FALSE) %>%
  ungroup()

ho_2025 <- preds_2025 %>%
  filter(implied_prob_A < HC_IMPLIED, edge >= HC_EDGE) %>%
  select(match_id, won_bet, odds_A, edge, implied_prob_A,
         surface = surface_clean, tourn_name, series)

cat(sprintf("  HC bets 2025: %d\n\n", nrow(ho_2025)))

# ════════════════════════════════════════════════════════════════════════════════
# PART C — Combine and summarise
# ════════════════════════════════════════════════════════════════════════════════

all_hc <- bind_rows(ho_2024, ho_2025)
cat(sprintf("Total HC bets 2020–2025: %d\n\n", nrow(all_hc)))

# Normalise level labels
all_hc <- all_hc %>%
  mutate(level = case_when(
    series == "Grand Slam"   ~ "Grand Slam",
    series == "Masters 1000" ~ "Masters 1000",
    series == "ATP500"       ~ "ATP 500",
    series == "ATP250"       ~ "ATP 250",
    TRUE                     ~ series   # e.g. "Masters Cup"
  ),
  surface = str_to_title(surface))

# ── Table 1: By tournament name ───────────────────────────────────────────────
by_tourn <- summarise_bets(all_hc, "tourn_name")

cat("══════════════════════════════════════════════════════════════════════\n")
cat("TABLE 1 — By tournament name  (edge > 0.15, implied < 0.29, 2020–2025)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")
print(as.data.frame(by_tourn), row.names = FALSE)

# ── Table 2: By tournament level ─────────────────────────────────────────────
by_level <- summarise_bets(all_hc, "level")

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("TABLE 2 — By tournament level  (edge > 0.15, implied < 0.29, 2020–2025)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")
print(as.data.frame(by_level), row.names = FALSE)

# ── Table 3: By surface ───────────────────────────────────────────────────────
by_surface <- summarise_bets(all_hc, "surface")

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("TABLE 3 — By surface  (edge > 0.15, implied < 0.29, 2020–2025)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")
print(as.data.frame(by_surface), row.names = FALSE)

# ── Save CSV ──────────────────────────────────────────────────────────────────
write_csv(by_tourn, "tournament_roi_breakdown.csv")
cat(sprintf("\nSaved: tournament_roi_breakdown.csv (%d rows)\n", nrow(by_tourn)))

cat("\nDone.\n")

# ── Interactive example ────────────────────────────────────────────────────────

if (FALSE) {
  setwd("C:/Users/User/OneDrive/Documents/tennis_model")
  source("startup.R"); source("features.R"); source("elo_features.R")
  source("tournament_analysis.R")
  # Results printed to console; by-tournament CSV saved to tournament_roi_breakdown.csv
}
