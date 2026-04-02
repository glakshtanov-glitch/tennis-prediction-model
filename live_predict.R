# live_predict.R
# Live tournament prediction pipeline.
# Fetches live rankings, parses today's draw, and produces match predictions.
# Depends on: startup.R, features.R, elo_features.R
#
# Primary model: model_inter3_elo
#   formula: outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog
#            + elo_diff_surface
#   Train: 2015-2019  |  Test AUC: 0.800  |  Validated 2020-2024
#
# High-confidence flag: edge > 0.15 AND implied_prob_A < 0.29
#   Backtest (2020-2024): ROI +63%, z=3.98, p<0.0001 (94 bets)

# ── Live rankings ──────────────────────────────────────────────────────────────
# Fetches current ATP rankings from the RapidAPI SofaScore endpoint.
# Stores result in live_rankings with columns: player, live_rank.

fetch_live_rankings <- function(api_key = RAPIDAPI_KEY) {
  url <- "https://tennisapi1.p.rapidapi.com/api/tennis/rankings/atp"
  response <- GET(url, add_headers(
    `x-rapidapi-host` = "tennisapi1.p.rapidapi.com",
    `x-rapidapi-key`  = api_key
  ))
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  data$rankings %>%
    transmute(
      player    = normalize_name(team.name),
      live_rank = ranking
    )
}

# ── Live match schedule ────────────────────────────────────────────────────────
# Fetches scheduled ATP matches for a given date from sportapi7.

fetch_matches <- function(date, api_key = RAPIDAPI_KEY) {
  Sys.sleep(0.5)   # be polite to the API
  url <- paste0("https://sportapi7.p.rapidapi.com/api/v1/sport/tennis/scheduled-events/", date)
  response <- GET(url, add_headers(
    `x-rapidapi-host` = "sportapi7.p.rapidapi.com",
    `x-rapidapi-key`  = api_key
  ))
  data <- fromJSON(content(response, "text"), flatten = TRUE)

  data$events %>%
    filter(
      tournament.category.name == "ATP",
      status.type == "finished",
      homeTeam.type == 1
    ) %>%
    select(
      date       = startTimestamp,
      winner_code = winnerCode,
      surface    = groundType,
      tournament = tournament.uniqueTournament.name,
      playerA    = homeTeam.name,
      playerB    = awayTeam.name
    ) %>%
    mutate(
      date    = as.Date(as.POSIXct(date, origin = "1970-01-01")),
      outcome = if_else(winner_code == 1, 1, 0)
    ) %>%
    filter(!is.na(outcome))
}

# ── Live odds ──────────────────────────────────────────────────────────────────

extract_odds <- function(match_idx, odds_data) {
  match      <- odds_data[match_idx, ]
  bookmakers <- match$bookmakers[[1]]
  map_dfr(1:nrow(bookmakers), function(i) {
    bm      <- bookmakers[i, ]
    markets <- bm$markets[[1]]
    h2h     <- markets %>% filter(key == "h2h")
    if (nrow(h2h) == 0) return(NULL)
    outcomes <- h2h$outcomes[[1]]
    outcomes %>% mutate(
      bookmaker     = bm$key,
      home_team     = match$home_team,
      away_team     = match$away_team,
      commence_time = match$commence_time
    )
  })
}

# ── Single-match prediction ────────────────────────────────────────────────────
# Primary model: model_inter3_elo
# Requires in environment: model_inter3_elo, live_rankings, elo_surface_lookup,
#   df_with_surface (for Elo history)
#
# tournament_matches: data frame of completed matches in the current tournament
#   with columns: winner, loser, score  (empty tibble for first-round matches)
#
# High-confidence flag: edge_A > 0.15 AND implied_A < 0.29

HIGH_CONF_EDGE     <- 0.15
HIGH_CONF_EDGE_CAP <- Inf    # no upper cap — high-edge bets outperform despite model overconfidence
HIGH_CONF_IMPLIED  <- 0.29
SS_MIN_STREAK_LIVE <- 2L    # consecutive straight-set wins to trigger SS signal

predict_match <- function(playerA, playerB, surface, tournament_matches,
                           odds_A = NA, odds_B = NA) {

  # ── 1. Rankings ─────────────────────────────────────────────────────────────
  rankA <- live_rankings %>% filter(player == playerA) %>% pull(live_rank)
  rankB <- live_rankings %>% filter(player == playerB) %>% pull(live_rank)

  if (length(rankA) == 0) { cat("Ranking not found:", playerA, "\n"); return(NULL) }
  if (length(rankB) == 0) { cat("Ranking not found:", playerB, "\n"); return(NULL) }

  # ── 2. Surface-specific Elo (current post-match rating from lookup) ─────────
  eloA <- get_player_surface_elo(playerA, surface, elo_surface_lookup)
  eloB <- get_player_surface_elo(playerB, surface, elo_surface_lookup)
  elo_diff_surface <- eloA - eloB

  # ── 3. Tournament momentum: games dominance from completed matches ──────────
  # Returns list(games_dom, ss_streak, matches_played); NA games_dom on R1.
  statsA <- get_player_tournament_stats(playerA, tournament_matches)
  statsB <- get_player_tournament_stats(playerB, tournament_matches)

  # games_dom_diff: NA → 0 (neutral) for first-round matches
  games_domA     <- if (is.na(statsA$games_dom)) 0.5 else statsA$games_dom
  games_domB     <- if (is.na(statsB$games_dom)) 0.5 else statsB$games_dom
  games_dom_diff <- games_domA - games_domB

  # games_dom_x_underdog = games_domA * I(rankA > rankB)  [rank-based underdog]
  # Confirmed formula from training data inspection.
  is_underdog_rank      <- as.numeric(rankA > rankB)
  games_dom_x_underdog  <- games_domA * is_underdog_rank

  # ── 4. model_inter3_elo prediction ──────────────────────────────────────────
  # formula: outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog
  #          + elo_diff_surface
  match_input <- data.frame(
    log_rank_ratio       = log(rankB / rankA),
    games_dom_diff       = games_dom_diff,
    games_dom_x_underdog = games_dom_x_underdog,
    elo_diff_surface     = elo_diff_surface
  )

  prob_A <- predict(model_inter3_elo, match_input, type = "prob")$.pred_1

  # ── 5. Assemble result ───────────────────────────────────────────────────────
  result <- tibble(
    playerA              = playerA,
    playerB              = playerB,
    surface              = surface,
    rankA                = rankA,
    rankB                = rankB,
    eloA                 = round(eloA, 1),
    eloB                 = round(eloB, 1),
    elo_diff_surface     = round(elo_diff_surface, 1),
    games_dom_diff       = round(games_dom_diff, 3),
    games_dom_x_underdog = round(games_dom_x_underdog, 3),
    ss_streakA           = statsA$ss_streak,
    ss_streakB           = statsB$ss_streak,
    prob_A               = round(prob_A, 4),
    prob_B               = round(1 - prob_A, 4),
    odds_A               = odds_A,
    odds_B               = odds_B
  )

  # ── 6. Edge and high-confidence flag (only when odds supplied) ───────────────
  if (!is.na(odds_A) && !is.na(odds_B)) {
    overround  <- 1/odds_A + 1/odds_B
    implied_A  <- (1/odds_A) / overround   # overround-adjusted implied prob
    implied_B  <- (1/odds_B) / overround
    edge_A     <- prob_A - implied_A

    result <- result %>% mutate(
      implied_A          = round(implied_A, 4),
      implied_B          = round(implied_B, 4),
      edge_A             = round(edge_A, 4),
      high_confidence_flag = edge_A > HIGH_CONF_EDGE &
                             edge_A < HIGH_CONF_EDGE_CAP &
                             implied_A < HIGH_CONF_IMPLIED
    )
  }

  result
}

# ── Day prediction pipeline ────────────────────────────────────────────────────
# matches_today: data frame with columns playerA, playerB, odds_A, odds_B.
# tournament_matches: completed matches in current tournament.
# Prints a summary flagging any HIGH CONFIDENCE bets.

predict_day <- function(matches_today, tournament_matches, surface = "Hard") {
  preds <- map_dfr(seq_len(nrow(matches_today)), function(i) {
    tryCatch(
      predict_match(
        playerA            = matches_today$playerA[i],
        playerB            = matches_today$playerB[i],
        surface            = surface,
        tournament_matches = tournament_matches,
        odds_A             = matches_today$odds_A[i],
        odds_B             = matches_today$odds_B[i]
      ),
      error = function(e) {
        cat("Error for", matches_today$playerA[i], "vs",
            matches_today$playerB[i], ":", e$message, "\n")
        NULL
      }
    )
  })

  if (nrow(preds) == 0) return(preds)

  # Print high-confidence flags if odds were supplied
  if ("high_confidence_flag" %in% names(preds)) {
    flags <- preds %>% filter(high_confidence_flag)
    if (nrow(flags) > 0) {
      cat("\n*** HIGH-CONFIDENCE BETS (edge >", HIGH_CONF_EDGE,
          "& edge <", HIGH_CONF_EDGE_CAP,
          "& implied <", HIGH_CONF_IMPLIED, ") ***\n")
      flags %>%
        select(playerA, playerB, prob_A, implied_A, edge_A,
               eloA, eloB, odds_A) %>%
        print()
      cat("\n")
    } else {
      cat("\n[No high-confidence bets at this threshold]\n\n")
    }
  }

  preds
}

# ── Daily report ──────────────────────────────────────────────────────────────
# Produces a ranked, annotated table of today's matches.
#
# Arguments:
#   tournament_matches : tibble(winner, loser, score) — completed matches in the
#                        current tournament (empty tibble for first-round day).
#   fixtures_today     : tibble(playerA, playerB, surface, odds_A, odds_B)
#
# Returns a tibble sorted by edge_A descending, with two extra signal columns:
#   high_confidence_flag — model edge: edge_A > 0.15 AND implied_A < 0.29
#   ss_signal            — SS rule: streaking underdog (ss_streak >= 2, rank underdog)
#
# Requires in environment: model_inter3_elo, live_rankings, elo_surface_lookup

daily_report <- function(tournament_matches, fixtures_today) {

  # Run predict_match for every fixture
  raw <- map_dfr(seq_len(nrow(fixtures_today)), function(i) {
    tryCatch(
      predict_match(
        playerA            = fixtures_today$playerA[i],
        playerB            = fixtures_today$playerB[i],
        surface            = fixtures_today$surface[i],
        tournament_matches = tournament_matches,
        odds_A             = fixtures_today$odds_A[i],
        odds_B             = fixtures_today$odds_B[i]
      ),
      error = function(e) {
        cat("Error for", fixtures_today$playerA[i], "vs",
            fixtures_today$playerB[i], ":", e$message, "\n")
        NULL
      }
    )
  })

  if (nrow(raw) == 0) return(raw)

  # ── SS Rule signal ───────────────────────────────────────────────────────────
  # Flags a match when a player has ss_streak >= SS_MIN_STREAK AND is an
  # underdog by ATP rank.  Both conditions required (mirrors the backtest rule).
  report <- raw %>%
    mutate(
      ss_signal = case_when(
        ss_streakA >= SS_MIN_STREAK_LIVE & rankA > rankB ~
          sprintf("A (%s, streak %d, rank %d)", playerA, ss_streakA, rankA),
        ss_streakB >= SS_MIN_STREAK_LIVE & rankB > rankA ~
          sprintf("B (%s, streak %d, rank %d)", playerB, ss_streakB, rankB),
        TRUE ~ NA_character_
      )
    )

  # ── Sort by edge descending ──────────────────────────────────────────────────
  if ("edge_A" %in% names(report)) {
    report <- report %>% arrange(desc(edge_A))
  }

  # ── Print ────────────────────────────────────────────────────────────────────
  cat(sprintf(
    "\n╔══════════════════════════════════════════════════════════════════╗\n"
  ))
  cat(sprintf(
    "║  DAILY REPORT  |  %d fixtures  |  edge threshold %.2f / implied < %.2f  ║\n",
    nrow(report), HIGH_CONF_EDGE, HIGH_CONF_IMPLIED
  ))
  cat(sprintf(
    "╚══════════════════════════════════════════════════════════════════╝\n\n"
  ))

  # Core prediction table
  display <- report %>%
    mutate(
      match      = sprintf("%-22s vs %-22s", playerA, playerB),
      surf       = substr(surface, 1, 2),
      ranks      = sprintf("%3d/%-3d", rankA, rankB),
      elo_d      = sprintf("%+.0f", elo_diff_surface),
      prob       = sprintf("%.3f", prob_A),
      imp        = sprintf("%.3f", implied_A),
      edge       = sprintf("%+.3f", edge_A),
      odds       = sprintf("%.2f", odds_A),
      hc         = if_else(high_confidence_flag, "*** HC ***", ""),
      ss         = if_else(!is.na(ss_signal), "SS", "")
    ) %>%
    select(match, surf, ranks, elo_d, prob, imp, edge, odds, hc, ss)

  print(as.data.frame(display), row.names = FALSE)

  # ── High-confidence section ──────────────────────────────────────────────────
  hc_bets <- report %>% filter(high_confidence_flag)
  if (nrow(hc_bets) > 0) {
    cat(sprintf(
      "\n*** HIGH-CONFIDENCE BETS (edge > %.2f AND implied < %.2f) ***\n",
      HIGH_CONF_EDGE, HIGH_CONF_IMPLIED
    ))
    hc_bets %>%
      mutate(label = sprintf(
        "  BET ON: %-20s  prob=%.3f  implied=%.3f  edge=%+.3f  odds=%.2f  elo_diff=%+.0f",
        playerA, prob_A, implied_A, edge_A, odds_A, elo_diff_surface
      )) %>%
      pull(label) %>%
      walk(cat, "\n")
  } else {
    cat("\n[No high-confidence bets today]\n")
  }

  # ── SS Rule section ──────────────────────────────────────────────────────────
  ss_bets <- report %>% filter(!is.na(ss_signal))
  if (nrow(ss_bets) > 0) {
    cat("\n--- SS Rule signals (streaking underdog, ss_streak >= 2) ---\n")
    ss_bets %>%
      mutate(label = sprintf(
        "  %s  |  streak_A=%d  streak_B=%d  prob_A=%.3f  edge=%+.3f",
        ss_signal, ss_streakA, ss_streakB, prob_A, edge_A
      )) %>%
      pull(label) %>%
      walk(cat, "\n")
  } else {
    cat("\n[No SS Rule signals today]\n")
  }

  cat("\n")
  invisible(report)
}

# ── Interactive example ────────────────────────────────────────────────────────

if (FALSE) {
  source("startup.R")
  source("features.R")
  source("elo_features.R")

  model_inter3_elo  <- readRDS("model_inter3_elo.rds")
  elo_surface_lookup <- readRDS("elo_surface_lookup.rds")
  live_rankings      <- fetch_live_rankings()

  # Empty tibble = first-round match (no prior tournament matches)
  tournament_matches <- tibble(winner = character(), loser = character(),
                                score = character())

  matches_today <- tribble(
    ~playerA,         ~playerB,          ~odds_A, ~odds_B,
    "Jannik Sinner",  "Carlos Alcaraz",  1.40,    3.00
  )

  preds <- predict_day(matches_today, tournament_matches, surface = "Hard")
  print(preds)
}
