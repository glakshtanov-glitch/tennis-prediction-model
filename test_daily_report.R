# test_daily_report.R
# End-to-end test of predict_match (high_confidence_flag rename) + daily_report.
# Fixtures are designed using real current ATP ranks so signals fire as expected.
#
# Known current ranks (from live_rankings):
#   Alcaraz=1, Sinner=2, Djokovic=3, Zverev=4, Musetti=5,
#   Medvedev=11, Ruud=12, Rune=27

setwd("C:/Users/User/OneDrive/Documents/tennis_model")
source("startup.R")
source("features.R")
source("elo_features.R")

model_inter3_elo   <- readRDS("model_inter3_elo.rds")
elo_surface_lookup <- readRDS("elo_surface_lookup.rds")
source("live_predict.R")

# ── Tournament history ─────────────────────────────────────────────────────────
# Alcaraz and Musetti both have 2-match straight-set streaks.
# Djokovic has 1 win (not a streak); Rune has no tournament history (R1-equivalent).

tournament_matches <- tibble(
  winner = c("Carlos Alcaraz",  "Carlos Alcaraz",   # 2-match SS streak → qualifies
             "Lorenzo Musetti", "Lorenzo Musetti",   # 2-match SS streak → qualifies
             "Novak Djokovic"),                      # 1 win, 3 sets → no streak
  loser  = c("Player A",        "Player B",
             "Player C",        "Player D",
             "Player E"),
  score  = c("6-3 6-2",         "6-4 6-1",          # straight sets
             "7-5 6-3",         "6-2 6-4",           # straight sets
             "7-6 4-6 6-4")                          # three sets — breaks streak
)

# ── Today's fixtures ───────────────────────────────────────────────────────────
# Match design rationale:
#
#  F1. Djokovic (rank 3) vs Sinner (rank 2) on Clay, huge odds against Djokovic
#      (odds_A=4.20). Djokovic has strong Clay Elo (~1972). implied_A ≈ 0.23 < 0.29.
#      Testing whether model edges up vs market → candidate for HC flag.
#
#  F2. Alcaraz (rank 1, SS streak) vs Medvedev (rank 11) on Hard.
#      Alcaraz is the rank favourite — SS rule must NOT fire (not an underdog).
#
#  F3. Sinner (rank 2) vs Musetti (rank 5) on Clay.
#      Musetti has SS streak=2. rankB (5) > rankA (2) → Musetti IS rank underdog.
#      SS rule MUST fire for playerB.
#
#  F4. Djokovic (rank 3) vs Rune (rank 27) on Clay, moderate odds.
#      No tournament history for Rune (first match). Tests first-round fallback.
#
#  F5. Zverev (rank 4) vs Ruud (rank 12) on Clay, close odds.
#      No special signals expected — tests the "quiet" case.

fixtures_today <- tribble(
  ~playerA,          ~playerB,          ~surface, ~odds_A, ~odds_B,
  "Novak Djokovic",  "Jannik Sinner",   "Clay",   4.20,    1.22,   # F1: Djokovic big underdog
  "Carlos Alcaraz",  "Daniil Medvedev", "Hard",   1.55,    2.50,   # F2: Alcaraz on SS, but fav
  "Jannik Sinner",   "Lorenzo Musetti", "Clay",   1.25,    4.50,   # F3: Musetti SS + rank underdog
  "Novak Djokovic",  "Holger Rune",     "Clay",   1.45,    2.80,   # F4: first-round Rune
  "Alexander Zverev","Casper Ruud",     "Clay",   1.80,    2.05    # F5: close match
)

# ── Run daily_report ───────────────────────────────────────────────────────────
cat("Running daily_report...\n")
report <- daily_report(tournament_matches, fixtures_today)

# ════════════════════════════════════════════════════════════════════
# CHECKS
# ════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════\n")
cat("AUTOMATED CHECKS\n")
cat("══════════════════════════════════════════════\n\n")

pass <- function(msg) cat(sprintf("PASS  %s\n", msg))
fail <- function(msg) { cat(sprintf("FAIL  %s\n", msg)); stop(msg) }

check <- function(cond, msg) if (cond) pass(msg) else fail(msg)

# 1. Column name
check("high_confidence_flag" %in% names(report),
      "high_confidence_flag column present")
check(!"high_conf" %in% names(report),
      "old 'high_conf' column absent")

# 2. SS signal fires for Musetti (F3: playerB, rankB=5 > rankA=2, streak=2)
musetti_row <- report %>% filter(playerA == "Jannik Sinner", playerB == "Lorenzo Musetti")
check(musetti_row$ss_streakB >= 2,
      sprintf("Musetti ss_streakB = %d (>= 2)", musetti_row$ss_streakB))
check(!is.na(musetti_row$ss_signal),
      sprintf("SS signal fires for Musetti: %s", musetti_row$ss_signal))

# 3. SS signal does NOT fire for Alcaraz (F2: Alcaraz is rank favourite, not underdog)
alcaraz_row <- report %>% filter(playerA == "Carlos Alcaraz")
check(alcaraz_row$rankA < alcaraz_row$rankB,
      sprintf("Alcaraz is rank favourite (rankA=%d < rankB=%d)",
              alcaraz_row$rankA, alcaraz_row$rankB))
check(is.na(alcaraz_row$ss_signal),
      "SS signal does NOT fire for Alcaraz (he's the rank favourite)")

# 4. Rune first-round: games_dom falls back to 0.5 / 0 (no prior matches)
rune_row <- report %>% filter(playerB == "Holger Rune")
check(!is.na(rune_row$prob_A),
      "Rune (no tournament history) produces a valid prediction")

# 5. Sorted by edge_A descending
check(all(diff(report$edge_A) <= 0),
      sprintf("Results sorted by edge_A descending: [%s]",
              paste(round(report$edge_A, 3), collapse = ", ")))

# 6. HC flag: inspect Djokovic row (F1 — high implied_A headroom)
djok_sinner_row <- report %>% filter(playerA == "Novak Djokovic", playerB == "Jannik Sinner")
cat(sprintf(
  "\nF1 Djokovic vs Sinner:  prob_A=%.3f  implied_A=%.3f  edge_A=%+.3f  HC=%s\n",
  djok_sinner_row$prob_A, djok_sinner_row$implied_A,
  djok_sinner_row$edge_A, djok_sinner_row$high_confidence_flag
))
cat("(HC fires when edge_A > 0.15 AND implied_A < 0.29 — noted above for reference)\n")

cat("\nAll checks passed.\n")
