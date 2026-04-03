# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

ATP tennis match prediction model built in R. Uses Jeff Sackmann's match data (2015–2024) with Bet365 historical odds for backtesting, and a RapidAPI live feed for in-tournament predictions.

## Running the code

**Automated daily pipeline (recommended):**
```bash
Rscript fetch_and_report.R
```
Fetches today's fixtures + completed results + odds end-to-end; outputs `daily_picks_YYYY-MM-DD.csv`.

**Interactive session** starts with:
```r
source("startup.R")   # loads libraries + restores tennis_project.RData
```

Scripts are sourced in dependency order:
```r
source("features.R")       # feature engineering functions
source("elo_features.R")   # surface Elo computation + live lookup
source("model.R")          # model fit/eval helpers
source("backtest.R")       # backtesting pipeline
source("live_predict.R")   # live prediction pipeline (uses model_inter3_elo)
source("ss_rule.R")        # straight-set streak rule
source("tracking.R")       # bet logging and P&L tracking
```

For live use, also load the saved artefacts:
```r
model_inter3_elo   <- readRDS("model_inter3_elo.rds")
elo_surface_lookup <- readRDS("elo_surface_lookup.rds")
live_rankings      <- fetch_live_rankings()
```

Each script has a guarded `if (FALSE) { ... }` block at the bottom with a self-contained interactive example — uncomment to run.

## Data locations

| Object | Description |
|--------|-------------|
| `df` | Full cleaned match dataset (27k+ rows, 63 cols) |
| `df_features6` | Primary feature table used for modelling (40 cols) |
| `odds_all` | Pooled Bet365 odds 2020–2024 |
| `live_rankings` | Current ATP rankings (fetched at session start) |

Raw Sackmann CSVs live in `C:/Users/User/OneDrive/Documents/Tennis 2015-2024/`.  
The full environment snapshot is `C:/Users/User/OneDrive/Documents/tennis_project.RData`.

## Data conventions

- **`outcome`**: `1` = playerA wins, `0` = playerB wins. Always a factor with levels `c(0, 1)` when passed to tidymodels.
- **playerA / playerB assignment**: playerA is the home team (`homeTeam.type == 1`) in the SofaScore feed; in historical Sackmann data, playerA = winner by convention in raw files, but the model dataset is constructed so either player can be A.
- **`tourney_date`** is stored as an integer in `YYYYMMDD` format — convert with `as.Date(as.character(x), format = "%Y%m%d")`.

## Feature engineering pipeline

Features are built incrementally (`df_features` → `df_features6`):

| Stage | Added features |
|-------|---------------|
| 1 | `formA/B`, `form_diff` — last-10 win rate |
| 2 | `wformA/B`, `wform_diff` — rank-weighted form (weight = 1/opp_rank) |
| 3 | `surf_wrA/B`, `surf_wr_diff` — surface win rate |
| 4 | `h2h_wrA/B`, `h2h_diff`, `h2h_v3` — H2H (see below) |
| 5 | `fatigueA/B`, `fatigue_diff` |
| 6 | `ss_streakA/B`, `games_domA/B`, `avg_minA/B` — tournament momentum |

Key derived column: `log_rank_ratio = log(rankB / rankA)` (used in all models).

**`h2h_v3` attenuation** (prevents overfitting on sparse H2H):
- `h2h_count >= 5` → use `h2h_diff` as-is
- `h2h_count >= 3` → use `h2h_diff * 0.5`
- `h2h_count < 3`  → use `0`

## Models

All models are `tidymodels` logistic regression (`_glm/model_fit`). The primary model for live predictions is **`model_inter3_elo`**.

| Object | Formula | Notes |
|--------|---------|-------|
| `model_inter3_elo` | `outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog + elo_diff_surface` | **Primary.** Train 2015–2019. AUC 0.801, Brier 0.183 |
| `model_inter3` | `outcome ~ log_rank_ratio + games_dom_diff + games_dom_x_underdog` | Predecessor, no Elo. AUC 0.691 |
| `model_wform` | `outcome ~ log_rank_ratio + wform_diff + surface` | Deprecated for live use |
| `model_best` | `outcome ~ log_rank_ratio + surf_wr_diff + h2h_v3` | Parsimonious baseline |
| `model_xgb` | XGBoost on `model_wform` feature set | Not competitive vs inter3_elo |

`model_inter3_elo` is saved as `model_inter3_elo.rds` in the project folder and also persisted in `tennis_project.RData`.

**High-confidence betting rule** (final, validated 2020–2024, 96 bets):
- `edge_A > 0.15` AND `implied_A < 0.29` — **no upper edge cap**
- Backtest: ROI +62.7%, win rate 38.5%, z=4.10, p<0.0001

An upper cap at `edge < 0.50` was tested and rejected. The 13 bets with edge ≥ 0.50 were the *best-performing* subset (61.5% win, +171% ROI, avg odds 4.51). Although the model is overconfident in raw probability in this region (see Calibration findings), the market is *even more wrong* — the true edge over the market (observed win rate − implied) remains large and genuine. Removing high-edge bets discards signal, not noise.

Constants in `live_predict.R`:
```r
HIGH_CONF_EDGE     <- 0.15
HIGH_CONF_EDGE_CAP <- Inf    # no cap — do not restrict high-edge bets
HIGH_CONF_IMPLIED  <- 0.29
```

To persist after retraining:
```r
saveRDS(model_inter3_elo, "model_inter3_elo.rds")
save.image(file = "C:/Users/User/OneDrive/Documents/tennis_project.RData")
```

## Calibration findings

Run `calibration.R` to reproduce. Key results on the 2020–2024 test set:

**Full test set (n=2,950):** REL=0.0017 — near-perfect global calibration.

**High-odds subset (implied < 0.29, n=378):** REL=0.0102. The model is overconfident in the upper deciles — it assigns 35–70% win probability to players the market prices at 5–29%. This is a **covariate shift** issue (extreme rank/Elo features in the tails) rather than a global flaw.

**Betting zone (edge > 0.15, implied < 0.29, n=96):**
- Mean predicted prob: 54.6% — mean observed win rate: 38.5%
- Calibration error: +16.1pp (model overpredicts)
- True edge over market (obs − implied): **+16.9pp** — this is the real number
- Edge retention ratio: ~50% (true edge / model-claimed edge)

**Interpretation:** The 0.15 threshold functions as a *model-relative selector*, not an absolute probability guarantee. The model's inflated probabilities still identify genuine market mispricings. Do not adjust the threshold based on overconfidence alone — the true edge is statistically validated (z=4.10). Critically, the overconfidence *increases* with edge magnitude but so does the market's error: bets with edge ≥ 0.50 (n=13) had 61.5% observed win rate vs 15.8% implied — the market was more wrong, not less.

**Platt scaling** (see `calibration.R`, scaler saved as `platt_scaler.rds`): Fitted on 5-fold CV of train_inter. Platt coefficients A=0.987, B=0.000 — nearly identity. Does not improve REL or edge retention in the high-odds region; the miscalibration is localised to the feature-space tails where training data is sparse, not a global sigmoid compression issue. Do not apply Platt scaling to live predictions.

## Surface Elo

`elo_surface_lookup.rds` — pre-match Elo for every match in `df_with_surface` (27,672 rows), computed with K=32, init=1500, one rating per player per surface (Hard/Clay/Grass/Carpet).

- Sorted strictly by `tourney_date + match_num` — no lookahead leakage (verified by independent replay in `verify_elo_leakage.R`)
- `get_player_surface_elo(player, surface, elo_lookup)` in `elo_features.R` returns the player's **current** post-match Elo (ready for the next prediction)
- `games_dom_x_underdog = games_domA * (rankA > rankB)` — rank-based, not odds-based (confirmed 100% match against training data)

## Walk-forward validation

`walk_forward.R` — 5 rolling windows, each retraining `model_inter3_elo` on growing history and testing on held-out year(s). Edge ≥ 0.15, implied < 0.29:

| Window | Test | Bets | Win% | ROI | p-value |
|--------|------|------|------|-----|---------|
| W1 | 2019 | 0 | — | — | — (no odds data) |
| W2 | 2020 | 8 | 37.5% | +76% | 0.151 |
| W3 | 2021 | 17 | 47.1% | +77% | 0.013 |
| W4 | 2022 | 22 | 31.8% | +40% | 0.110 |
| W5 | 2023–24 | 47 | 36.2% | +58% | 0.003 |
| 2025 OOS | 2025 | 42 | 23.8% | +5.5% | 0.318 |

Five of six windows show positive ROI. W3 and W5 are individually significant. Small per-window samples explain the non-significant windows — the cumulative sample is now 136 bets across 2020–2025.

## 2025 out-of-sample backtest

Run by `update_2025.R`. Extends `elo_surface_lookup.rds` with 2508 completed 2025 matches from tennis-data.co.uk. Model and training data (2015–2019) unchanged. Edge ≥ 0.15, implied < 0.29:

| Year | Bets | Win% | ROI | z | p-value |
|------|------|------|-----|---|---------|
| 2025 | 42 | 23.8% | +5.5% | 0.47 | 0.318 |

Not individually significant (consistent with walk-forward windows of comparable size). ROI is positive. The 2025 CSV covers the full ATP season including 250-level events, producing a larger high-odds pool (35.8% of matched matches have an underdog with implied < 0.29) vs the 2020–2024 major-tournament-weighted backtest. Cumulative signal (2020–2025, ~138 bets) remains positive.

**Threshold sweep (2025, implied < 0.29):**

| Threshold | n bets | Win rate | ROI | z | p-value |
|-----------|--------|----------|-----|---|---------|
| edge > 0.05 | 109 | 21.1% | −6.5% | 0.27 | 0.394 |
| edge > 0.10 | 74 | 20.3% | −5.6% | −0.002 | 0.501 |
| **edge > 0.15** | **42** | **23.8%** | **+5.5%** | **0.47** | **0.318** |
| edge > 0.20 | 30 | 26.7% | +21.3% | 0.61 | 0.272 |
| edge > 0.25 | 19 | 31.6% | +47.4% | 1.08 | 0.141 |

## API dependencies

Keys must be in `.Renviron` (never hardcoded):
```
RAPIDAPI_KEY=...
ODDS_API_KEY=...
```

- **sportapi7.p.rapidapi.com** — scheduled match results and live draws
- **tennisapi1.p.rapidapi.com** — live ATP rankings
- **The Odds API** — pre-match odds

## Straight-set streak rule

A player triggers the rule when `ss_streak >= SS_MIN_STREAK` (default 2), where `ss_streak` counts consecutive straight-set tournament wins going into the current match. The rule bets on the streaking player when `edge >= SS_MIN_EDGE`. See `bets_ss` in the environment for pre-computed historical results.

## Automated live pipeline (fetch_and_report.R)

Single-command daily pipeline. Requires `RAPIDAPI_KEY` and `ODDS_API_KEY` in `.Renviron`.

```bash
Rscript fetch_and_report.R
```

**Steps:**
1. `fetch_live_rankings()` — ATP rankings from tennisapi1
2. `fetch_completed_atp(dates = today - 0:2)` — completed ATP results from sportapi7; extracts per-set scores from `homeScore.period1..5` / `awayScore.period1..5` to build `winner/loser/score` (score = NA → games_dom defaults to 0.5)
3. `fetch_scheduled_atp(today)` — upcoming fixtures (`status.type == "notstarted"`), all concurrent ATP events
4. `fetch_odds_today()` — The Odds API (`tennis_atp`, `eu` region, `h2h`, decimal odds); prefers Bet365, falls back to first available bookmaker
5. `attach_odds_to_fixtures()` — name-normalized match between sportapi7 and Odds API players
6. Per-tournament predict loop — for each tournament group, builds its own `tournament_matches` and calls `predict_match()` directly; combined report sorted by edge descending
7. Saves `daily_picks_YYYY-MM-DD.csv` (today's run) and appends to `daily_picks.csv` (running log)

**Graceful degradation:** each API step is `tryCatch`-wrapped. Completed-results failure → neutral features (games_dom = 0.5). Odds failure → predictions printed without HC flags.

**Output columns:** `fetched_at, tournament, playerA, playerB, surface, rankA, rankB, eloA, eloB, elo_diff_surface, games_dom_diff, games_dom_x_underdog, ss_streakA, ss_streakB, prob_A, prob_B, odds_A, odds_B, implied_A, implied_B, edge_A, high_confidence_flag, ss_signal`

## Design Decisions

Key architectural choices and their empirical justification. Do not revisit without new evidence.

**Logistic regression over XGBoost:** `model_xgb` (XGBoost on the `model_wform` feature set) was tested and did not outperform `model_inter3_elo`. With only 3,651 training rows after NA-filtering for Elo features, logistic regression generalises better than gradient boosting on small data; its linear decision boundary is also more stable in the sparse tail region where the betting rule operates.

**Upper edge cap rejected:** An `edge < 0.50` cap was explicitly tested. The 13 removed bets (edge ≥ 0.50) had 61.5% win rate and +171% ROI vs 15.8% implied — the best-performing subset of the 94. At extreme edges, the model is overconfident in raw probability, but the market's error is proportionally larger. Removing these bets discards genuine signal. Consequence: `HIGH_CONF_EDGE_CAP <- Inf`.

**Platt scaling not applied to live predictions:** Platt coefficients A=0.987, B=0.000 (near identity) when fitted on 5-fold CV of `train_inter`. The +16.3pp miscalibration in the betting zone is localised to the feature-space tails (extreme rank ratios and Elo differences sparse in training data) — a covariate-shift problem that a global affine recalibration cannot fix. No improvement in REL or edge retention in the high-odds region. Scaler saved as `platt_scaler.rds` but not applied.

**Odds not used as a model feature:** `odds_all` covers only 2020–2024; the training period is 2015–2019 with no odds coverage. Including market odds as a model input would require either discarding 5 years of training data or training a model that cannot be evaluated on its own training set. Edge is computed post-prediction as `p̂ − implied`, keeping model signal and market signal cleanly separate.

**Rank-based Elo initialisation adopted:** Instead of flat 1500 for every player's first match on a surface, new players are initialised at `1500 + 100 * (1 - log(rank) / log(500))` capped to [1400, 1700] — giving top-ranked players a head start (rank 1 → 1600, rank 500 → 1500, rank 750 → 1493). Tested in `elo_init_experiment.R` on the 2015–2024 dataset. Result: AUC 0.801 (+0.001), ROI +62.7% (+0.2pp), 96 bets (vs 94). Marginal but non-negative on both criteria; artefacts updated. The rank used is each player's ATP ranking at their first recorded match on that surface in the Sackmann dataset.

## Null result experiments

Features tested and discarded — do not re-test without new justification.

| Feature / Approach | Script | AUC delta | ROI delta (edge>0.15) | Coefficient p | Verdict |
|---------|--------|-----------|----------------------|---------------|---------|
| `round_num` (ordinal R128=1…F=7) | `round_feature_experiment.R` | +0.0000 | +0.0% | p=0.942 | Discard — collinear with rank/Elo/games_dom |
| XGBoost (`model_xgb`, `model_wform` features) | — | < 0.800 | < +62.5% | — | Discard — logistic generalises better on 3,651 train rows |
| `wform_diff` (`model_wform`) | — | — | < inter3_elo | — | Deprecated — Elo captures form signal more stably; superseded |
| Serve stats (1st serve%, aces, break points) | — | — | — | — | Discard — no incremental AUC beyond rank and Elo |
| Age / trajectory | — | — | — | — | Discard — no predictive signal beyond rank and Elo |
| `fatigue_diff` (standalone) | — | — | — | — | Discard — subsumed by games_dom; not in any retained model |

**`round_num` (April 2026):** Tournament round encoded as integer 1–7 (R128→F). Coefficient = −0.002 (p = 0.942). AUC and ROI on 2020–2024 test set identical to baseline to 4 decimal places. The same 94 high-confidence bets are selected at the 0.15 threshold. Round is structurally correlated with rank/Elo/games_dom — it carries no independent predictive signal once those features are in the model.

## Name normalisation

Player names from different sources need normalisation before joining:
- `normalize_name(x)` — strips accents (stringi)
- `abbreviate_name(x)` — converts "First Surname" → "Surname F." (used to match Bet365 format)
