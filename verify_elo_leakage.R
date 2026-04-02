setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("elo_features.R")

# ── Load saved artefacts ──────────────────────────────────────────────────────
elo_lookup     <- readRDS("elo_surface_lookup.rds")
model_inter3_elo <- readRDS("model_inter3_elo.rds")

cat("elo_surface_lookup rows:", nrow(elo_lookup), "\n")
cat("model_inter3_elo class:", class(model_inter3_elo), "\n\n")

# ── Build chronologically ordered master table ────────────────────────────────
# df_with_surface is the raw Sackmann data (27,672 matches, 2015-2024)
# We sort strictly by tourney_date then match_num to reproduce the exact
# order the Elo was computed in.

matches_ordered <- df_with_surface %>%
  arrange(tourney_date, match_num) %>%
  mutate(row_order = row_number()) %>%
  left_join(elo_lookup, by = "match_id")

# ── Pick 3 audit matches from test_inter (2020-2024) ─────────────────────────
# Criteria: well-known players, different surfaces, different years,
# enough prior history that Elo is clearly non-trivial (not near 1500).

audit_matches <- bind_rows(
  # 1. Hard court, 2021 — Novak Djokovic (should have very high Hard Elo)
  matches_ordered %>%
    filter(surface == "Hard",
           as.integer(substr(as.character(tourney_date), 1, 4)) == 2021,
           winner_name == "Novak Djokovic" | loser_name == "Novak Djokovic") %>%
    slice(8),   # 8th Hard match Djokovic plays in 2021

  # 2. Clay court, 2023 — Carlos Alcaraz
  matches_ordered %>%
    filter(surface == "Clay",
           as.integer(substr(as.character(tourney_date), 1, 4)) == 2023,
           winner_name == "Carlos Alcaraz" | loser_name == "Carlos Alcaraz") %>%
    slice(5),

  # 3. Grass court, 2022 — Novak Djokovic at Wimbledon
  matches_ordered %>%
    filter(surface == "Grass",
           as.integer(substr(as.character(tourney_date), 1, 4)) == 2022,
           winner_name == "Novak Djokovic" | loser_name == "Novak Djokovic") %>%
    slice(4)
)

cat("══════════════════════════════════════════════════════════════\n")
cat("AUDIT MATCHES SELECTED\n")
cat("══════════════════════════════════════════════════════════════\n")
audit_matches %>%
  select(row_order, tourney_date, tourney_name, surface, round,
         winner_name, loser_name, elo_winner_pre, elo_loser_pre) %>%
  print(width = 120)

# ── Verification function ─────────────────────────────────────────────────────
# For a given row_order, independently replays every prior same-surface match
# from scratch (K=32, init=1500) to recompute both players' Elo, then
# compares against the stored values.

verify_elo_match <- function(target_row_order, matches_ordered, K = 32, init = 1500) {

  target  <- matches_ordered %>% filter(row_order == target_row_order)
  surf    <- target$surface
  winner  <- target$winner_name
  loser   <- target$loser_name

  cat("\n════════════════════════════════════════════════════════════\n")
  cat(sprintf("MATCH %d: %s vs %s\n", target_row_order, winner, loser))
  cat(sprintf("Date: %s  |  Tournament: %s  |  Surface: %s  |  Round: %s\n",
              target$tourney_date, target$tourney_name, surf, target$round))
  cat(sprintf("Stored  elo_winner_pre : %.6f\n", target$elo_winner_pre))
  cat(sprintf("Stored  elo_loser_pre  : %.6f\n", target$elo_loser_pre))

  # ── Last 5 prior same-surface matches for each player ────────────────────
  prior_surf <- matches_ordered %>%
    filter(row_order < target_row_order, surface == surf)

  show_history <- function(player) {
    prior_surf %>%
      filter(winner_name == player | loser_name == player) %>%
      arrange(row_order) %>%
      tail(5) %>%
      mutate(
        result   = if_else(winner_name == player, "WIN ", "LOSS"),
        opponent = if_else(winner_name == player, loser_name, winner_name)
      ) %>%
      select(row_order, tourney_date, tournament = tourney_name,
             opponent, result)
  }

  cat(sprintf("\n  Last ≤5 %s matches for %s  (all row_order < %d):\n",
              surf, winner, target_row_order))
  w_hist <- show_history(winner)
  if (nrow(w_hist) == 0) {
    cat("  [no prior matches on this surface — Elo will equal", init, "]\n")
  } else {
    print(w_hist, n = 5)
  }

  cat(sprintf("\n  Last ≤5 %s matches for %s  (all row_order < %d):\n",
              surf, loser, target_row_order))
  l_hist <- show_history(loser)
  if (nrow(l_hist) == 0) {
    cat("  [no prior matches on this surface — Elo will equal", init, "]\n")
  } else {
    print(l_hist, n = 5)
  }

  # ── Independent Elo replay from scratch ──────────────────────────────────
  # Replay every same-surface match BEFORE this one using a fresh environment.
  all_prior_surf <- prior_surf %>% arrange(row_order)

  elo_env <- new.env(hash = TRUE, parent = emptyenv())
  get_e   <- function(p) { v <- elo_env[[p]]; if (is.null(v)) init else v }

  for (i in seq_len(nrow(all_prior_surf))) {
    w  <- all_prior_surf$winner_name[i]
    l  <- all_prior_surf$loser_name[i]
    ew <- get_e(w);  el <- get_e(l)
    xw <- 1 / (1 + 10^((el - ew) / 400))
    elo_env[[w]] <- ew + K * (1 - xw)
    elo_env[[l]] <- el + K * (0 - (1 - xw))
  }

  computed_w <- get_e(winner)
  computed_l <- get_e(loser)

  cat(sprintf("\n  %-6s  %-30s  computed = %.6f  |  stored = %.6f  |  %s\n",
              "WINNER", winner, computed_w, target$elo_winner_pre,
              if (abs(computed_w - target$elo_winner_pre) < 1e-6) "✓ EXACT MATCH"
              else sprintf("✗ DIFF = %.2e", abs(computed_w - target$elo_winner_pre))))

  cat(sprintf("  %-6s  %-30s  computed = %.6f  |  stored = %.6f  |  %s\n",
              "LOSER", loser, computed_l, target$elo_loser_pre,
              if (abs(computed_l - target$elo_loser_pre) < 1e-6) "✓ EXACT MATCH"
              else sprintf("✗ DIFF = %.2e", abs(computed_l - target$elo_loser_pre))))

  list(winner_ok = abs(computed_w - target$elo_winner_pre) < 1e-6,
       loser_ok  = abs(computed_l - target$elo_loser_pre) < 1e-6)
}

# ── Run all three ─────────────────────────────────────────────────────────────
cat("\n\n")
results <- lapply(audit_matches$row_order, verify_elo_match,
                  matches_ordered = matches_ordered)

# ── Final verdict ─────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("LEAKAGE AUDIT VERDICT\n")
cat("══════════════════════════════════════════════════════════════\n")
for (i in seq_along(results)) {
  m <- audit_matches[i, ]
  w_ok <- results[[i]]$winner_ok
  l_ok <- results[[i]]$loser_ok
  cat(sprintf("  Match %d (%s vs %s, %s): winner %s | loser %s\n",
              i, m$winner_name, m$loser_name, m$surface,
              if (w_ok) "✓" else "✗",
              if (l_ok) "✓" else "✗"))
}
all_ok <- all(sapply(results, function(r) r$winner_ok && r$loser_ok))
cat(sprintf("\nAll 6 values exact: %s\n",
            if (all_ok) "YES — no lookahead leakage confirmed"
            else "NO — investigate mismatches"))
