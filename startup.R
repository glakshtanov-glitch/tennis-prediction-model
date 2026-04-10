# startup.R
# Load libraries and restore the project environment.
# Run this first at the start of every session.

library(tidyverse)
library(tidymodels)
library(lubridate)
library(stringi)
library(httr)
library(jsonlite)
library(rvest)
library(xgboost)

# ── Environment ────────────────────────────────────────────────────────────────
RDATA_PATH <- "C:/Users/User/OneDrive/Documents/tennis_project.RData"
DATA_DIR   <- "C:/Users/User/OneDrive/Documents/Tennis 2015-2024"

load(RDATA_PATH)
message("Environment loaded from: ", RDATA_PATH)
message("Objects in workspace: ", length(ls()))

# ── API keys ───────────────────────────────────────────────────────────────────
# .Renviron is not always auto-loaded when R is invoked via Rscript from certain
# shells (e.g. Git Bash on Windows). Force-load from both known locations so
# keys are always available regardless of working directory or shell context.
for (.rv in c(file.path(path.expand("~"), ".Renviron"),
              "C:/Users/User/OneDrive/Documents/.Renviron")) {
  if (file.exists(.rv)) readRenviron(.rv)
}
rm(.rv)

RAPIDAPI_KEY <- Sys.getenv("RAPIDAPI_KEY")
ODDS_API_KEY <- Sys.getenv("ODDS_API_KEY")

# ── Constants ──────────────────────────────────────────────────────────────────
TOUR_AVG_FIRST_IN       <- 0.62
TOUR_AVG_SERVE_WIN      <- 0.72
TOUR_AVG_SURF_GAMES_DOM <- 0.50
