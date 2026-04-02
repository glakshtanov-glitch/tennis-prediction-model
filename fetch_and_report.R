# fetch_and_report.R
# Automated daily prediction pipeline — runs end-to-end with a single Rscript call.
#
# Steps:
#   1. Fetch completed ATP results for the last 3 days (all active tournaments)
#   2. Parse into tournament_matches format (winner/loser/score)
#   3. Fetch today's scheduled ATP fixtures
#   4. Fetch current odds from The Odds API
#   5. Run per-tournament predictions via predict_match()
#   6. Print combined ranked report with HC + SS signals
#   7. Save to daily_picks_YYYY-MM-DD.csv and append to daily_picks.csv
#
# Usage:
#   Rscript fetch_and_report.R
#
# Requires in .Renviron: RAPIDAPI_KEY, ODDS_API_KEY
# Requires in working dir: model_inter3_elo.rds, elo_surface_lookup.rds

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")
source("live_predict.R")

# ── Helper: extract per-set score string from sportapi7 flattened events ────────
# Tries homeScore.period1..5 / awayScore.period1..5 (standard SofaScore JSON).
# Returns "w1-l1 w2-l2 ..." from winner's perspective, or NA if fields absent.

extract_score_string <- function(events_df) {
  pmap_chr(events_df, function(...) {
    row         <- list(...)
    winner_code <- row[["winnerCode"]]
    if (is.null(winner_code) || is.na(winner_code)) return(NA_character_)

    sets <- character(0)
    for (p in 1:5) {
      h_val <- row[[paste0("homeScore.period", p)]]
      a_val <- row[[paste0("awayScore.period", p)]]
      if (is.null(h_val) || is.null(a_val)) break
      if (is.na(h_val)   || is.na(a_val))   break
      h <- as.integer(h_val)
      a <- as.integer(a_val)
      sets <- c(sets,
        if (winner_code == 1) sprintf("%d-%d", h, a)   # home won
        else                  sprintf("%d-%d", a, h))   # away won
    }

    if (length(sets) == 0) NA_character_
    else paste(sets, collapse = " ")
  })
}

# ── Step 1: Fetch completed ATP results (last 3 days, all tournaments) ──────────

fetch_completed_atp <- function(dates = Sys.Date() - 0:2,
                                 api_key = RAPIDAPI_KEY) {
  map_dfr(as.character(dates), function(d) {
    tryCatch({
      Sys.sleep(0.5)
      url  <- paste0("https://sportapi7.p.rapidapi.com/api/v1/sport/tennis/scheduled-events/", d)
      resp <- GET(url, add_headers(
        `x-rapidapi-host` = "sportapi7.p.rapidapi.com",
        `x-rapidapi-key`  = api_key
      ))
      data   <- fromJSON(content(resp, "text"), flatten = TRUE)
      events <- data$events
      if (is.null(events) || nrow(events) == 0) return(NULL)

      # Filter to completed ATP matches with a determined winner
      atp_done <- events %>%
        filter(
          tournament.category.name == "ATP",
          status.type              == "finished",
          homeTeam.type            == 1,
          !is.na(winnerCode)
        )
      if (nrow(atp_done) == 0) return(NULL)

      atp_done %>%
        mutate(
          match_date = as.Date(as.POSIXct(startTimestamp, origin = "1970-01-01")),
          outcome    = if_else(winnerCode == 1, 1L, 0L),
          winner     = if_else(winnerCode == 1, homeTeam.name, awayTeam.name),
          loser      = if_else(winnerCode == 1, awayTeam.name, homeTeam.name),
          score      = extract_score_string(.)
        ) %>%
        transmute(
          match_date,
          tournament = tournament.uniqueTournament.name,
          surface    = groundType,
          playerA    = normalize_name(homeTeam.name),
          playerB    = normalize_name(awayTeam.name),
          winner     = normalize_name(winner),
          loser      = normalize_name(loser),
          outcome,
          score
        )
    }, error = function(e) {
      cat(sprintf("  [warn] fetch_completed_atp %s: %s\n", d, e$message))
      NULL
    })
  })
}

# ── Step 2: Build tournament_matches tibble from completed results ───────────────
# tournament_matches: tibble(winner, loser, score) — consumed by predict_match()

build_tournament_matches <- function(completed_df) {
  if (is.null(completed_df) || nrow(completed_df) == 0)
    return(tibble(winner = character(), loser = character(), score = character()))
  completed_df %>% select(winner, loser, score)
}

# ── Step 3: Fetch today's scheduled (not-yet-started) ATP fixtures ───────────────

fetch_scheduled_atp <- function(date = Sys.Date(), api_key = RAPIDAPI_KEY) {
  tryCatch({
    Sys.sleep(0.5)
    url  <- paste0("https://sportapi7.p.rapidapi.com/api/v1/sport/tennis/scheduled-events/", date)
    resp <- GET(url, add_headers(
      `x-rapidapi-host` = "sportapi7.p.rapidapi.com",
      `x-rapidapi-key`  = api_key
    ))
    data   <- fromJSON(content(resp, "text"), flatten = TRUE)
    events <- data$events
    if (is.null(events) || nrow(events) == 0) return(NULL)

    upcoming <- events %>%
      filter(
        tournament.category.name == "ATP",
        status.type              %in% c("notstarted", "postponed"),
        homeTeam.type            == 1
      )
    if (nrow(upcoming) == 0) return(NULL)

    upcoming %>%
      transmute(
        tournament = tournament.uniqueTournament.name,
        surface    = groundType,
        playerA    = normalize_name(homeTeam.name),
        playerB    = normalize_name(awayTeam.name)
      )
  }, error = function(e) {
    cat(sprintf("  [warn] fetch_scheduled_atp: %s\n", e$message))
    NULL
  })
}

# ── Step 4: Fetch odds from The Odds API ─────────────────────────────────────────
# Returns raw parsed list; NULL on any error.

fetch_odds_today <- function(api_key = ODDS_API_KEY) {
  if (is.na(api_key) || api_key == "") {
    cat("  [warn] ODDS_API_KEY not set — skipping odds fetch\n")
    return(NULL)
  }
  tryCatch({
    resp <- GET(
      "https://api.the-odds-api.com/v4/sports/tennis_atp/odds/",
      query = list(
        apiKey     = api_key,
        regions    = "eu",
        markets    = "h2h",
        oddsFormat = "decimal"
      )
    )
    if (status_code(resp) != 200) {
      cat(sprintf("  [warn] Odds API returned HTTP %d\n", status_code(resp)))
      return(NULL)
    }
    fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = FALSE)
  }, error = function(e) {
    cat(sprintf("  [warn] fetch_odds_today: %s\n", e$message))
    NULL
  })
}

# ── Step 4b: Extract h2h odds for a single fixture from Odds API response ────────
# Tries bookmaker preferred_bm first, falls back to first available.
# Returns list(odds_A, odds_B) or list(NA_real_, NA_real_) if no match.

get_odds_for_fixture <- function(pA_norm, pB_norm, odds_raw,
                                  preferred_bm = "bet365") {
  no_odds <- list(odds_A = NA_real_, odds_B = NA_real_)
  if (is.null(odds_raw) || length(odds_raw) == 0) return(no_odds)

  # odds_raw is a data frame; each row is an event
  n_events <- if (is.data.frame(odds_raw)) nrow(odds_raw) else length(odds_raw)

  matched_idx <- NA_integer_
  home_is_A   <- NA

  for (i in seq_len(n_events)) {
    ht <- if (is.data.frame(odds_raw)) odds_raw$home_team[i] else odds_raw[[i]]$home_team
    at <- if (is.data.frame(odds_raw)) odds_raw$away_team[i] else odds_raw[[i]]$away_team
    if (is.null(ht) || is.null(at)) next
    ht_norm <- normalize_name(ht)
    at_norm <- normalize_name(at)
    if (ht_norm == pA_norm && at_norm == pB_norm) {
      matched_idx <- i; home_is_A <- TRUE;  break
    }
    if (ht_norm == pB_norm && at_norm == pA_norm) {
      matched_idx <- i; home_is_A <- FALSE; break
    }
  }

  if (is.na(matched_idx)) return(no_odds)

  bms <- if (is.data.frame(odds_raw)) odds_raw$bookmakers[[matched_idx]]
         else                         odds_raw[[matched_idx]]$bookmakers
  if (is.null(bms) || (is.data.frame(bms) && nrow(bms) == 0)) return(no_odds)

  # Prefer preferred_bm; fallback to first
  if (is.data.frame(bms)) {
    bm_row <- bms %>% filter(key == preferred_bm)
    if (nrow(bm_row) == 0) bm_row <- bms[1, ]
    markets <- bm_row$markets[[1]]
  } else {
    bm_keys <- map_chr(bms, "key")
    bm_idx  <- which(bm_keys == preferred_bm)
    if (length(bm_idx) == 0) bm_idx <- 1L
    markets <- bms[[bm_idx]]$markets
  }

  if (is.null(markets)) return(no_odds)

  # Extract h2h outcomes
  if (is.data.frame(markets)) {
    h2h_row <- markets %>% filter(key == "h2h")
    if (nrow(h2h_row) == 0) return(no_odds)
    outcomes <- h2h_row$outcomes[[1]]
  } else {
    mkt_keys <- map_chr(markets, "key")
    h2h_idx  <- which(mkt_keys == "h2h")
    if (length(h2h_idx) == 0) return(no_odds)
    outcomes <- markets[[h2h_idx]]$outcomes
  }

  if (is.null(outcomes) || nrow(outcomes) == 0) return(no_odds)
  out_norms <- normalize_name(outcomes$name)

  idx_A <- which(out_norms == pA_norm)
  idx_B <- which(out_norms == pB_norm)
  if (length(idx_A) == 0 || length(idx_B) == 0) return(no_odds)

  list(odds_A = outcomes$price[idx_A[1]],
       odds_B = outcomes$price[idx_B[1]])
}

attach_odds_to_fixtures <- function(fixtures, odds_raw) {
  if (is.null(fixtures) || nrow(fixtures) == 0) return(fixtures)
  odds_cols <- map_dfr(seq_len(nrow(fixtures)), function(i) {
    res <- get_odds_for_fixture(fixtures$playerA[i], fixtures$playerB[i], odds_raw)
    tibble(odds_A = res$odds_A, odds_B = res$odds_B)
  })
  bind_cols(fixtures, odds_cols)
}

# ── Step 5 + 6: Per-tournament predict loop + combined report print ───────────────
# Groups fixtures by tournament, passes each group its own tournament_matches,
# calls predict_match() directly, combines and prints a single ranked table.

run_combined_report <- function(all_completed, fixtures_today) {
  if (is.null(fixtures_today) || nrow(fixtures_today) == 0) {
    cat("\n[No upcoming fixtures found for today]\n")
    return(invisible(tibble()))
  }

  tournaments <- unique(fixtures_today$tournament)
  cat(sprintf("\nRunning predictions across %d tournament(s): %s\n",
              length(tournaments), paste(tournaments, collapse = ", ")))

  raw <- map_dfr(tournaments, function(t_name) {
    tm <- build_tournament_matches(
      if (!is.null(all_completed)) all_completed %>% filter(tournament == t_name)
      else NULL
    )
    cat(sprintf("  %s — %d prior matches in history\n", t_name, nrow(tm)))

    t_fix <- fixtures_today %>% filter(tournament == t_name)

    map_dfr(seq_len(nrow(t_fix)), function(i) {
      tryCatch(
        predict_match(
          playerA            = t_fix$playerA[i],
          playerB            = t_fix$playerB[i],
          surface            = t_fix$surface[i],
          tournament_matches = tm,
          odds_A             = t_fix$odds_A[i],
          odds_B             = t_fix$odds_B[i]
        ) %>% mutate(tournament = t_name),
        error = function(e) {
          cat(sprintf("  [warn] predict_match %s vs %s: %s\n",
                      t_fix$playerA[i], t_fix$playerB[i], e$message))
          NULL
        }
      )
    })
  })

  if (nrow(raw) == 0) {
    cat("\n[No predictions generated — check player name matching in live_rankings]\n")
    return(invisible(tibble()))
  }

  # ── SS signal (mirrors daily_report logic) ───────────────────────────────────
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

  if ("edge_A" %in% names(report)) {
    report <- report %>% arrange(desc(edge_A))
  }

  # ── Print ────────────────────────────────────────────────────────────────────
  has_odds <- "edge_A" %in% names(report)
  run_ts   <- format(Sys.time(), "%Y-%m-%d %H:%M")

  cat(sprintf(
    "\n╔══════════════════════════════════════════════════════════════════╗\n"
  ))
  cat(sprintf(
    "║  DAILY REPORT  |  %s  |  %d fixtures  |  edge > %.2f / implied < %.2f  ║\n",
    run_ts, nrow(report),
    HIGH_CONF_EDGE, HIGH_CONF_IMPLIED
  ))
  cat(sprintf(
    "╚══════════════════════════════════════════════════════════════════╝\n\n"
  ))

  if (has_odds) {
    display <- report %>%
      mutate(
        match  = sprintf("%-22s vs %-22s", playerA, playerB),
        tourny = substr(tournament, 1, 12),
        surf   = substr(surface, 1, 2),
        ranks  = sprintf("%3d/%-3d", rankA, rankB),
        elo_d  = sprintf("%+.0f", elo_diff_surface),
        prob   = sprintf("%.3f", prob_A),
        imp    = sprintf("%.3f", implied_A),
        edge   = sprintf("%+.3f", edge_A),
        odds   = sprintf("%.2f", odds_A),
        hc     = if_else(high_confidence_flag, "*** HC ***", ""),
        ss     = if_else(!is.na(ss_signal), "SS", "")
      ) %>%
      select(match, tourny, surf, ranks, elo_d, prob, imp, edge, odds, hc, ss)
    print(as.data.frame(display), row.names = FALSE)
  } else {
    display <- report %>%
      mutate(
        match  = sprintf("%-22s vs %-22s", playerA, playerB),
        tourny = substr(tournament, 1, 12),
        surf   = substr(surface, 1, 2),
        ranks  = sprintf("%3d/%-3d", rankA, rankB),
        elo_d  = sprintf("%+.0f", elo_diff_surface),
        prob   = sprintf("%.3f", prob_A),
        ss     = if_else(!is.na(ss_signal), "SS", "")
      ) %>%
      select(match, tourny, surf, ranks, elo_d, prob, ss)
    print(as.data.frame(display), row.names = FALSE)
    cat("\n[Odds unavailable — high-confidence flags not shown]\n")
  }

  # ── High-confidence section ──────────────────────────────────────────────────
  if (has_odds) {
    hc_bets <- report %>% filter(high_confidence_flag)
    if (nrow(hc_bets) > 0) {
      cat(sprintf(
        "\n*** HIGH-CONFIDENCE BETS (edge > %.2f AND implied < %.2f) ***\n",
        HIGH_CONF_EDGE, HIGH_CONF_IMPLIED
      ))
      hc_bets %>%
        mutate(label = sprintf(
          "  BET ON: %-20s  [%s]  prob=%.3f  implied=%.3f  edge=%+.3f  odds=%.2f  elo_diff=%+.0f",
          playerA, tournament, prob_A, implied_A, edge_A, odds_A, elo_diff_surface
        )) %>%
        pull(label) %>%
        walk(cat, "\n")
    } else {
      cat("\n[No high-confidence bets today]\n")
    }
  }

  # ── SS signal section ────────────────────────────────────────────────────────
  ss_bets <- report %>% filter(!is.na(ss_signal))
  if (nrow(ss_bets) > 0) {
    cat("\n--- SS Rule signals (streaking underdog, ss_streak >= 2) ---\n")
    ss_bets %>%
      mutate(label = sprintf(
        "  %s  |  [%s]  streak_A=%d  streak_B=%d  prob_A=%.3f%s",
        ss_signal, tournament, ss_streakA, ss_streakB, prob_A,
        if (has_odds) sprintf("  edge=%+.3f", edge_A) else ""
      )) %>%
      pull(label) %>%
      walk(cat, "\n")
  } else {
    cat("\n[No SS Rule signals today]\n")
  }

  cat("\n")
  invisible(report)
}

# ── Step 7: Save to CSV ───────────────────────────────────────────────────────────

save_picks <- function(report, date_str = as.character(Sys.Date())) {
  if (nrow(report) == 0) {
    cat("[No predictions to save]\n")
    return(invisible(NULL))
  }

  out <- report %>%
    mutate(fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) %>%
    select(fetched_at, tournament, playerA, playerB, surface,
           rankA, rankB, eloA, eloB, elo_diff_surface,
           games_dom_diff, games_dom_x_underdog,
           ss_streakA, ss_streakB,
           prob_A, prob_B,
           any_of(c("odds_A", "odds_B", "implied_A", "implied_B",
                     "edge_A", "high_confidence_flag")),
           ss_signal)

  # Dated file — clean, single-run
  dated_path <- sprintf("daily_picks_%s.csv", date_str)
  write_csv(out, dated_path)
  cat(sprintf("Saved: %s (%d rows)\n", dated_path, nrow(out)))

  # Running log — append
  log_path <- "daily_picks.csv"
  if (file.exists(log_path)) {
    write_csv(out, log_path, append = TRUE, col_names = FALSE)
  } else {
    write_csv(out, log_path)
  }
  cat(sprintf("Appended: %s\n", log_path))
}

# ════════════════════════════════════════════════════════════════════════════════
# MAIN PIPELINE
# ════════════════════════════════════════════════════════════════════════════════

model_inter3_elo   <- readRDS("model_inter3_elo.rds")
elo_surface_lookup <- readRDS("elo_surface_lookup.rds")

cat("\n── Fetching live rankings ──────────────────────────────────────\n")
live_rankings <- tryCatch(
  fetch_live_rankings(),
  error = function(e) {
    cat(sprintf("[FAIL] Rankings API error: %s\n", e$message))
    NULL
  }
)
if (is.null(live_rankings) || nrow(live_rankings) == 0) {
  cat("\n[ABORT] Cannot fetch live rankings (API quota exhausted or key invalid).\n")
  cat("        Predictions require current ATP rankings — cannot continue.\n")
  cat("        Check RAPIDAPI_KEY in .Renviron and quota at rapidapi.com\n\n")
  quit(save = "no", status = 0)
}
cat(sprintf("Rankings loaded: %d players\n", nrow(live_rankings)))

today     <- Sys.Date()
date_str  <- as.character(today)
past_3    <- today - 0:2

cat(sprintf("\n── Fetching completed results (%s to %s) ───────────────────────\n",
            as.character(min(past_3)), date_str))

all_completed <- fetch_completed_atp(dates = past_3)

if (!is.null(all_completed) && nrow(all_completed) > 0) {
  completed_summary <- all_completed %>%
    count(tournament, name = "matches") %>%
    arrange(desc(matches))
  cat(sprintf("Completed matches found: %d across %d tournament(s)\n",
              nrow(all_completed), nrow(completed_summary)))
  print(as.data.frame(completed_summary), row.names = FALSE)
} else {
  cat("No completed matches found in the last 3 days.\n")
  all_completed <- NULL
}

cat(sprintf("\n── Fetching today's scheduled fixtures (%s) ─────────────────────\n",
            date_str))

scheduled <- fetch_scheduled_atp(date = today)

if (is.null(scheduled) || nrow(scheduled) == 0) {
  cat("No upcoming ATP fixtures found for today. Exiting.\n")
  quit(status = 0)
}

cat(sprintf("%d fixture(s) scheduled today:\n", nrow(scheduled)))
print(scheduled %>% select(tournament, playerA, playerB, surface) %>% as.data.frame(),
      row.names = FALSE)

cat("\n── Fetching odds from The Odds API ─────────────────────────────────────\n")

odds_raw <- fetch_odds_today()

if (!is.null(odds_raw)) {
  n_events <- if (is.data.frame(odds_raw)) nrow(odds_raw) else length(odds_raw)
  cat(sprintf("Odds API: %d event(s) returned\n", n_events))
} else {
  cat("Odds unavailable — predictions will run without HC flags\n")
}

cat("\n── Matching odds to fixtures ────────────────────────────────────────────\n")

fixtures_today <- attach_odds_to_fixtures(scheduled, odds_raw)

n_matched <- sum(!is.na(fixtures_today$odds_A))
cat(sprintf("%d / %d fixtures matched to odds\n", n_matched, nrow(fixtures_today)))

# ── Run predictions ───────────────────────────────────────────────────────────

report <- run_combined_report(all_completed, fixtures_today)

# ── Save ──────────────────────────────────────────────────────────────────────

save_picks(report, date_str = date_str)

cat("Done.\n")
