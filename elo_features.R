# elo_features.R
# Compute rolling surface-specific Elo ratings from Sackmann match data.
# Elo is computed on ALL matches in df_with_surface (27k+) in strict
# chronological order (tourney_date + match_num). Pre-match Elo is recorded
# so there is no lookahead leakage.
#
# Returns a tibble with match_id | elo_winner_pre | elo_loser_pre
# Join to any feature table on match_id, then compute elo_diff_surface.
#
# Depends on: df_with_surface in the loaded environment.

compute_surface_elo <- function(
    matches_df,
    K         = 32,
    init_elo  = 1500
) {
  # Sort strictly by date then match_num within tournament
  matches_sorted <- matches_df %>%
    arrange(tourney_date, match_num) %>%
    mutate(row_idx = row_number())

  n <- nrow(matches_sorted)

  # One hash-environment per surface for O(1) player lookups
  surf_levels <- c("Hard", "Clay", "Grass", "Carpet")
  elo_envs <- setNames(
    lapply(surf_levels, function(s) new.env(hash = TRUE, parent = emptyenv())),
    surf_levels
  )

  elo_winner_pre <- numeric(n)
  elo_loser_pre  <- numeric(n)

  get_elo <- function(env, player) {
    v <- env[[player]]
    if (is.null(v)) init_elo else v
  }

  for (i in seq_len(n)) {
    surf   <- matches_sorted$surface[i]
    winner <- matches_sorted$winner_name[i]
    loser  <- matches_sorted$loser_name[i]

    # Skip if surface not in our set (shouldn't happen, but be safe)
    env <- elo_envs[[surf]]
    if (is.null(env)) {
      elo_winner_pre[i] <- init_elo
      elo_loser_pre[i]  <- init_elo
      next
    }

    elo_w <- get_elo(env, winner)
    elo_l <- get_elo(env, loser)

    # Record pre-match ratings
    elo_winner_pre[i] <- elo_w
    elo_loser_pre[i]  <- elo_l

    # Expected win probability (logistic scale, 400-point spread)
    exp_w <- 1 / (1 + 10^((elo_l - elo_w) / 400))

    # Update
    env[[winner]] <- elo_w + K * (1 - exp_w)
    env[[loser]]  <- elo_l + K * (0 - (1 - exp_w))
  }

  matches_sorted %>%
    mutate(
      elo_winner_pre = elo_winner_pre,
      elo_loser_pre  = elo_loser_pre
    ) %>%
    select(match_id, elo_winner_pre, elo_loser_pre)
}


# ── Build and join ─────────────────────────────────────────────────────────────
# Adds elo_diff_surface to any feature table that has:
#   - match_id (joinable to df_with_surface)
#   - playerA / playerB (to determine which is winner)
#
# df_with_surface has winner_name / loser_name which we use to orient elo_diff.

# ── Current Elo lookup for live prediction ────────────────────────────────────
# Returns a player's current surface Elo by replaying the update from their
# most recent match. This is the Elo they carry INTO their next match.
# Requires df_with_surface and elo_lookup to be in the calling environment,
# or passed explicitly.

get_player_surface_elo <- function(player, surface, elo_lookup,
                                    matches_df = df_with_surface,
                                    K = 32, init = 1500) {
  hist <- matches_df %>%
    filter(surface == !!surface,
           winner_name == player | loser_name == player) %>%
    arrange(tourney_date, match_num) %>%
    left_join(elo_lookup, by = "match_id") %>%
    mutate(
      is_winner   = winner_name == player,
      elo_self    = if_else(is_winner, elo_winner_pre, elo_loser_pre),
      elo_opp     = if_else(is_winner, elo_loser_pre,  elo_winner_pre)
    )

  if (nrow(hist) == 0) return(init)

  last      <- tail(hist, 1)
  exp_w     <- 1 / (1 + 10^((last$elo_opp - last$elo_self) / 400))
  delta     <- if_else(last$is_winner, K * (1 - exp_w), K * (0 - (1 - exp_w)))
  last$elo_self + delta
}

# ── add_elo_feature ───────────────────────────────────────────────────────────
add_elo_feature <- function(feat_df, elo_lookup) {
  # elo_lookup: match_id | elo_winner_pre | elo_loser_pre
  # feat_df has playerA = winner when outcome == 1
  # We need: elo_A (pre-match Elo for playerA on this surface)
  #           elo_B (pre-match Elo for playerB on this surface)
  #
  # Since the Sackmann raw data always lists winner first,
  # elo_winner_pre = Elo of the player who WON the match.
  # playerA == winner iff outcome == 1.

  feat_df %>%
    left_join(elo_lookup, by = "match_id") %>%
    mutate(
      outcome_num    = as.integer(as.character(outcome)),
      elo_A          = if_else(outcome_num == 1, elo_winner_pre, elo_loser_pre),
      elo_B          = if_else(outcome_num == 1, elo_loser_pre,  elo_winner_pre),
      elo_diff_surface = elo_A - elo_B
    ) %>%
    select(-elo_winner_pre, -elo_loser_pre, -outcome_num)
}
