# ATP Tennis Prediction Model

Logistic regression model that identifies statistically significant edges on high-odds ATP underdogs. Trained on 2015–2019 Sackmann match data and validated out-of-sample on 2020–2024 (AUC 0.800, 94 high-confidence bets, +62.5% ROI, z = 3.98, p < 0.0001). The full pipeline is automated: a single `Rscript fetch_and_report.R` call fetches live rankings, current fixtures, and Bet365 odds, then outputs a ranked prediction file. First live deployment: Monte-Carlo Masters, April 2026 (clay).

---

## 1. Model specification

**Model:** `model_inter3_elo` — logistic regression (tidymodels/glm), trained 2015–2019.

```
outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface
```

| Feature | Description |
|---------|-------------|
| `log_rank_ratio` | log(rankB / rankA) — log-scaled rank asymmetry |
| `games_dom_diff` | In-tournament games dominance differential (momentum) |
| `games_dom_x_underdog` | Momentum × underdog interaction (rank-based) |
| `elo_diff_surface` | Surface-specific Elo difference (clay/hard/grass, K=32, no lookahead) |

**Betting rule:** `edge > 0.15` AND `implied_prob < 0.29` — no upper edge cap.  
Edge = model probability − bookmaker implied probability.

---

## 2. How to run

**Daily pipeline (recommended):**
```bash
Rscript fetch_and_report.R
```
Outputs `daily_picks_YYYY-MM-DD.csv` and appends to `daily_picks.csv`.

**Interactive session:**
```r
source("startup.R")      # loads libraries + tennis_project.RData
source("features.R")
source("elo_features.R")
source("live_predict.R")
model_inter3_elo   <- readRDS("model_inter3_elo.rds")
elo_surface_lookup <- readRDS("elo_surface_lookup.rds")
live_rankings      <- fetch_live_rankings()
```

**Logging a live bet:**
```r
source("tracking.R")
log_bet(playerA = "Tsitsipas S.", playerB = "Cerundolo F.",
        tournament = "Monte-Carlo Masters", surface = "Clay", round = "R32",
        signal_type = "HC", model_prob = 0.44, implied_prob = 0.22,
        edge = 0.22, odds_taken = 4.50)
update_result(playerA = "Tsitsipas S.", result = "W")
pnl_summary()
```

---

## 3. Data sources

| Source | Used for |
|--------|---------|
| [Jeff Sackmann ATP match data](https://github.com/JeffSackmann/tennis_atp) | Historical match results 2015–2024 |
| [tennis-data.co.uk](http://www.tennis-data.co.uk/) | Bet365 historical odds 2020–2024 (backtesting) |
| [The Odds API](https://the-odds-api.com/) | Live Bet365 odds (daily pipeline) |
| [RapidAPI / tennisapi1](https://rapidapi.com/sportapi/api/tennisapi1/) | Live ATP rankings |
| [RapidAPI / sportapi7](https://rapidapi.com/sportapi/api/sportapi7/) | Live fixtures and completed results |

Add API keys to `~/.Renviron`:
```
RAPIDAPI_KEY=your_key_here
ODDS_API_KEY=your_key_here
```

---

## 4. Backtest results — threshold sensitivity (2020–2024, high-odds underdogs)

`model_inter3_elo` at the validated rule (edge > 0.15, implied < 0.29):

| Threshold | n bets | Win rate | ROI | Avg odds | z-score | p-value |
|-----------|--------|----------|-----|----------|---------|---------|
| edge > 0.05 | 154 | 28.6% | +22.3% | — | — | 0.006 |
| edge > 0.10 | 116 | 34.5% | +46.0% | 4.38 | 3.41 | 0.0002 |
| **edge > 0.15** | **94** | **38.3%** | **+62.5%** | **4.54** | **3.98** | **<0.0001** |
| edge > 0.20 | 76 | 39.5% | +68.0% | 4.72 | 3.62 | 0.0001 |
| edge > 0.25 | 59 | 39.0% | +64.3% | 4.89 | 3.31 | 0.0004 |

All competing models (logistic without Elo, XGBoost, straight-set rule) showed negative ROI at every threshold on high-odds underdogs.

---

## 5. Walk-forward validation

Model retrained from scratch in each window; test period held out entirely. Edge > 0.15, implied < 0.29.

| Window | Test period | Bets | Win rate | ROI | p-value |
|--------|-------------|------|----------|-----|---------|
| W2 | 2020 | 8 | 37.5% | +76% | 0.151 |
| W3 | 2021 | 17 | 47.1% | +77% | 0.013 |
| W4 | 2022 | 22 | 31.8% | +40% | 0.110 |
| W5 | 2023–2024 | 47 | 36.2% | +58% | 0.003 |

All four windows are positive ROI. Two are individually significant; cumulative z = 3.98.

---

## 6. Limitations

- **Small sample:** 94 bets over five years; variance is high and individual-year results fluctuate.
- **Calibration:** Model overestimates raw win probability in the betting zone by ~16pp; the validated edge is market-relative, not absolute.
- **Odds availability:** The Odds API covers major tournaments; qualifying and smaller 250-level events may have no odds returned.
- **Name normalisation:** Mismatches between SofaScore and Odds API player names can cause fixtures to run without odds (graceful degradation, not a crash).
- **Surface Elo cold start:** New players and those returning from injury have sparse Elo histories; the model defaults to 1500 (average).

---

## 7. Project structure

| File | Purpose |
|------|---------|
| `fetch_and_report.R` | Daily pipeline — fetch → features → predict → CSV |
| `live_predict.R` | Live prediction functions (`predict_match`, `run_combined_report`) |
| `tracking.R` | Live P&L logging (`log_bet`, `update_result`, `pnl_summary`) |
| `features.R` | Historical feature engineering pipeline |
| `elo_features.R` | Surface Elo computation and live lookup |
| `model.R` | Model training and evaluation helpers |
| `backtest.R` | Historical backtesting pipeline |
| `walk_forward.R` | Walk-forward cross-validation |
| `calibration.R` | Calibration analysis (REL, Platt scaling assessment) |
| `startup.R` | Session initialisation |
| `model_inter3_elo.rds` | Saved primary model |
| `elo_surface_lookup.rds` | Pre-computed surface Elo ratings (27k+ matches) |
| `live_bets_log.csv` | Live bet log — appended to during each tournament |
| `CLAUDE.md` | Full technical documentation (design decisions, null results) |
