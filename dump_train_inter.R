# dump_train_inter.R
# Export train_inter, test_inter, and odds_all (2020-2024 only) as CSV files.
# Purpose: allow a collaborator to reproduce model_inter3_elo in Python and
# verify feature parity.
#
# Outputs (written to project directory):
#   train_inter.csv        — 2015-2019 training rows (all columns)
#   test_inter.csv         — 2020-2024 test rows     (all columns)
#   odds_2020_2024.csv     — Bet365 odds filtered to 2020-2024

setwd("C:/Users/User/OneDrive/Documents/tennis_model")

library(tidyverse)

RDATA_PATH <- "C:/Users/User/OneDrive/Documents/tennis_project.RData"
cat("Loading environment from:", RDATA_PATH, "\n")
load(RDATA_PATH)

# ── 1. train_inter ────────────────────────────────────────────────────────────

stopifnot(exists("train_inter"))
cat(sprintf("train_inter: %d rows x %d cols\n", nrow(train_inter), ncol(train_inter)))

train_years <- range(as.integer(substr(as.character(train_inter$tourney_date), 1, 4)))
cat(sprintf("  Years: %d – %d\n", train_years[1], train_years[2]))

write_csv(train_inter, "train_inter.csv")
cat("  -> train_inter.csv written\n\n")

# ── 2. test_inter ─────────────────────────────────────────────────────────────

stopifnot(exists("test_inter"))
cat(sprintf("test_inter: %d rows x %d cols\n", nrow(test_inter), ncol(test_inter)))

test_years <- range(as.integer(substr(as.character(test_inter$tourney_date), 1, 4)))
cat(sprintf("  Years: %d – %d\n", test_years[1], test_years[2]))

write_csv(test_inter, "test_inter.csv")
cat("  -> test_inter.csv written\n\n")

# ── 3. odds_all: inspect and filter to 2020-2024 ─────────────────────────────

stopifnot(exists("odds_all"))
cat(sprintf("odds_all: %d rows x %d cols\n", nrow(odds_all), ncol(odds_all)))

# Date column is "M/D/YYYY" — extract year
odds_years_raw <- odds_all %>%
  mutate(
    year = as.integer(map_chr(str_split(Date, "/"), ~ .x[3]))
  )

year_counts <- odds_years_raw %>%
  count(year, name = "n_matches") %>%
  arrange(year)

cat("  Year breakdown in odds_all:\n")
print(year_counts, n = Inf)

# Filter to 2020-2024 (exclude any 2025 or earlier data outside that range)
odds_2020_2024 <- odds_years_raw %>%
  filter(year >= 2020, year <= 2024) %>%
  select(-year)   # drop the helper column; keep original columns intact

cat(sprintf("\n  After filtering to 2020-2024: %d rows\n", nrow(odds_2020_2024)))

if (nrow(odds_2020_2024) == 0) {
  stop("No rows remain after filtering odds_all to 2020-2024 — check Date column format.")
}

write_csv(odds_2020_2024, "odds_2020_2024.csv")
cat("  -> odds_2020_2024.csv written\n\n")

cat("Done. Three files ready for Python handoff:\n")
cat("  train_inter.csv\n")
cat("  test_inter.csv\n")
cat("  odds_2020_2024.csv\n")
