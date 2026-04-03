# tracking.R
# Live P&L tracking for the Monte-Carlo and subsequent ATP tournament deployments.
# Provides log_bet() to append a single row to live_bets_log.csv from the R console.
# Depends on: startup.R (for tidyverse)

# ── Constants ──────────────────────────────────────────────────────────────────

BETS_LOG_PATH <- "live_bets_log.csv"

# Column order used by live_bets_log.csv
BETS_LOG_COLS <- c(
  "date", "tournament", "playerA", "playerB", "surface", "round",
  "signal_type", "model_prob", "implied_prob", "edge",
  "odds_taken", "bookmaker", "stake", "result", "profit",
  "cumulative_profit", "notes"
)

# ── log_bet() ─────────────────────────────────────────────────────────────────
# Append one bet to live_bets_log.csv.
#
# Arguments:
#   playerA      : character — the player the model flagged (A in pipeline output)
#   playerB      : character — the opponent
#   tournament   : character — e.g. "Monte-Carlo Masters"
#   surface      : character — "Clay" / "Hard" / "Grass"
#   round        : character — e.g. "R64", "R32", "R16", "QF", "SF", "F"
#   signal_type  : character — "HC", "SS", or "both"
#   model_prob   : numeric   — prob_A from pipeline output
#   implied_prob : numeric   — implied_A from pipeline output
#   edge         : numeric   — edge_A from pipeline output
#   odds_taken   : numeric   — decimal odds actually obtained at time of bet
#   bookmaker    : character — e.g. "Bet365", "Pinnacle"
#   stake        : numeric   — units staked (use 1 for flat staking)
#   result       : character — "W", "L", or "void" (fill in after match)
#   notes        : character — optional free-text (late scratch, retirement, etc.)
#   date         : Date      — defaults to today
#   log_path     : character — path to the CSV log file
#
# Returns: invisibly, the row appended (as a tibble).
# Side effect: appends one row to log_path. Creates file with header if absent.

log_bet <- function(playerA,
                    playerB,
                    tournament,
                    surface,
                    round,
                    signal_type,
                    model_prob,
                    implied_prob,
                    edge,
                    odds_taken,
                    bookmaker    = "Bet365",
                    stake        = 1,
                    result       = NA_character_,
                    notes        = NA_character_,
                    date         = Sys.Date(),
                    log_path     = BETS_LOG_PATH) {

  # ── Validate signal_type ────────────────────────────────────────────────────
  if (!signal_type %in% c("HC", "SS", "both")) {
    stop("signal_type must be one of: 'HC', 'SS', 'both'")
  }

  # ── Validate result if provided ─────────────────────────────────────────────
  if (!is.na(result) && !result %in% c("W", "L", "void")) {
    stop("result must be 'W', 'L', 'void', or NA")
  }

  # ── Compute profit from result ───────────────────────────────────────────────
  # profit = (odds_taken - 1) * stake on win, -stake on loss, 0 on void
  profit <- if (is.na(result)) {
    NA_real_
  } else if (result == "W") {
    round((odds_taken - 1) * stake, 4)
  } else if (result == "L") {
    -stake
  } else {  # void
    0
  }

  # ── Compute cumulative profit from existing log ──────────────────────────────
  cumulative_profit <- tryCatch({
    if (file.exists(log_path)) {
      existing  <- read_csv(log_path, col_types = cols(.default = "c"), show_col_types = FALSE)
      prev_sum  <- sum(as.numeric(existing$profit), na.rm = TRUE)
      if (!is.na(profit)) prev_sum + profit else NA_real_
    } else {
      if (!is.na(profit)) profit else NA_real_
    }
  }, error = function(e) {
    cat(sprintf("  [warn] Could not compute cumulative_profit: %s\n", e$message))
    NA_real_
  })

  # ── Build the new row ────────────────────────────────────────────────────────
  new_row <- tibble(
    date              = as.character(date),
    tournament        = as.character(tournament),
    playerA           = as.character(playerA),
    playerB           = as.character(playerB),
    surface           = as.character(surface),
    round             = as.character(round),
    signal_type       = as.character(signal_type),
    model_prob        = round(as.numeric(model_prob),   4),
    implied_prob      = round(as.numeric(implied_prob), 4),
    edge              = round(as.numeric(edge),         4),
    odds_taken        = round(as.numeric(odds_taken),   2),
    bookmaker         = as.character(bookmaker),
    stake             = as.numeric(stake),
    result            = as.character(result),
    profit            = profit,
    cumulative_profit = cumulative_profit,
    notes             = as.character(notes)
  )

  # ── Write to CSV (create with header if absent, otherwise append) ────────────
  tryCatch({
    if (!file.exists(log_path)) {
      write_csv(new_row, log_path)
      cat(sprintf("[log_bet] Created %s — first entry logged.\n", log_path))
    } else {
      write_csv(new_row, log_path, append = TRUE, col_names = FALSE)
      cat(sprintf("[log_bet] Appended to %s\n", log_path))
    }
  }, error = function(e) {
    cat(sprintf("[ERROR] Could not write to %s: %s\n", log_path, e$message))
  })

  invisible(new_row)
}

# ── update_result() ───────────────────────────────────────────────────────────
# Fill in the result and profit for a previously logged bet (matched by
# playerA + playerB + date). Use this after the match finishes.
#
# Arguments:
#   playerA   : character — must match the logged row exactly
#   playerB   : character
#   result    : "W", "L", or "void"
#   date      : Date — defaults to today
#   log_path  : path to the CSV log

update_result <- function(playerA,
                          result,
                          playerB   = NULL,
                          date      = Sys.Date(),
                          log_path  = BETS_LOG_PATH) {

  if (!result %in% c("W", "L", "void")) {
    stop("result must be 'W', 'L', or 'void'")
  }
  if (!file.exists(log_path)) {
    stop(sprintf("Log file not found: %s", log_path))
  }

  log <- read_csv(log_path, col_types = cols(.default = "c"), show_col_types = FALSE)

  # Identify matching row(s)
  mask <- log$playerA == playerA & log$date == as.character(date)
  if (!is.null(playerB)) mask <- mask & log$playerB == playerB

  n_match <- sum(mask, na.rm = TRUE)
  if (n_match == 0) {
    cat(sprintf("[warn] No matching row found for %s on %s\n", playerA, date))
    return(invisible(NULL))
  }
  if (n_match > 1) {
    cat(sprintf("[warn] %d rows matched — updating the first. Supply playerB to disambiguate.\n", n_match))
    mask <- which(mask)[1]
  }

  # Compute profit
  odds_taken <- as.numeric(log$odds_taken[mask])
  stake      <- as.numeric(log$stake[mask])

  profit <- if (result == "W")    round((odds_taken - 1) * stake, 4)
            else if (result == "L") -stake
            else 0

  log$result[mask] <- result
  log$profit[mask] <- as.character(profit)

  # Recompute all cumulative_profit values in order
  profits <- as.numeric(log$profit)
  log$cumulative_profit <- as.character(cumsum(ifelse(is.na(profits), 0, profits)))

  write_csv(log, log_path)
  cat(sprintf("[update_result] Updated %s vs %s → %s (profit: %+.2f units)\n",
              playerA, if (!is.null(playerB)) playerB else "?", result, profit))

  invisible(log[mask, ])
}

# ── pnl_summary() ────────────────────────────────────────────────────────────
# Print a quick P&L summary from the log file.

pnl_summary <- function(log_path = BETS_LOG_PATH) {
  if (!file.exists(log_path)) {
    cat("[pnl_summary] Log file not found.\n")
    return(invisible(NULL))
  }

  log <- read_csv(log_path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(
      profit     = as.numeric(profit),
      odds_taken = as.numeric(odds_taken),
      stake      = as.numeric(stake),
      model_prob = as.numeric(model_prob),
      edge       = as.numeric(edge)
    )

  settled <- log %>% filter(!is.na(profit), result != "void")

  if (nrow(settled) == 0) {
    cat("[pnl_summary] No settled bets yet.\n")
    return(invisible(NULL))
  }

  cat("\n══ Live P&L Summary ════════════════════════════════════════════\n")
  cat(sprintf("  Bets logged   : %d  (settled: %d, pending: %d, void: %d)\n",
              nrow(log),
              nrow(settled),
              sum(is.na(log$result) | log$result == "NA", na.rm = TRUE),
              sum(log$result == "void", na.rm = TRUE)))
  cat(sprintf("  Win rate      : %.1f%% (%d / %d)\n",
              100 * mean(settled$result == "W"),
              sum(settled$result == "W"),
              nrow(settled)))
  cat(sprintf("  Total profit  : %+.2f units\n", sum(settled$profit, na.rm = TRUE)))
  cat(sprintf("  ROI           : %+.1f%%\n",
              100 * sum(settled$profit, na.rm = TRUE) / sum(settled$stake, na.rm = TRUE)))
  cat(sprintf("  Avg odds      : %.2f\n", mean(settled$odds_taken, na.rm = TRUE)))
  cat(sprintf("  Avg edge      : %+.3f\n", mean(settled$edge, na.rm = TRUE)))
  cat("════════════════════════════════════════════════════════════════\n\n")

  by_signal <- settled %>%
    group_by(signal_type) %>%
    summarise(
      n        = n(),
      wins     = sum(result == "W"),
      profit   = sum(profit),
      roi      = profit / sum(stake),
      .groups  = "drop"
    )
  print(as.data.frame(by_signal), row.names = FALSE)
  cat("\n")

  invisible(settled)
}

# ── Interactive example ────────────────────────────────────────────────────────

if (FALSE) {
  source("startup.R")
  source("tracking.R")

  # Log a bet at the time of placing it (result unknown yet)
  log_bet(
    playerA      = "Tsitsipas S.",
    playerB      = "Cerundolo F.",
    tournament   = "Monte-Carlo Masters",
    surface      = "Clay",
    round        = "R32",
    signal_type  = "HC",
    model_prob   = 0.44,
    implied_prob = 0.22,
    edge         = 0.22,
    odds_taken   = 4.50,
    bookmaker    = "Bet365",
    stake        = 1
  )

  # After the match, fill in the result
  update_result(playerA = "Tsitsipas S.", result = "W", date = Sys.Date())

  # Check current P&L
  pnl_summary()
}
