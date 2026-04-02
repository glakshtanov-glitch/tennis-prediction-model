# ATP Tennis Prediction Model

Logistic regression model for ATP match prediction, trained on 2015–2019 Sackmann data and validated on 2020–2024.

## Key results (2020–2024 test set)

| Metric | Value |
|--------|-------|
| AUC | 0.800 |
| Brier score | 0.183 |
| High-confidence bets (edge > 0.15, implied < 0.29) | 94 bets, +62.5% ROI |
| Statistical significance | z = 3.98, p < 0.0001 |

Model features: log rank ratio, tournament games dominance, surface Elo difference.

## Setup

**Requirements:** R 4.x, tidyverse, tidymodels, httr, jsonlite

```r
install.packages(c("tidyverse", "tidymodels", "httr", "jsonlite",
                   "parsnip", "yardstick", "stringi", "patchwork"))
```

Add API keys to `~/.Renviron`:
```
RAPIDAPI_KEY=your_key_here
ODDS_API_KEY=your_key_here
```

Restore the model environment (`.RData` file not in repo — run `retrain_and_validate.R` to rebuild):
```r
source("startup.R")
source("features.R")
source("elo_features.R")
```

## Daily predictions

```bash
Rscript fetch_and_report.R
```

Fetches live rankings, completed results, and odds automatically. Outputs `daily_picks_YYYY-MM-DD.csv`.

## Full documentation

See [`CLAUDE.md`](CLAUDE.md) for complete technical documentation: feature engineering pipeline, model formulas, betting rule derivation, calibration findings, walk-forward validation results, and null result experiments.
