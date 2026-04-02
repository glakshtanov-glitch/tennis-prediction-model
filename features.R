# features.R
# All feature engineering functions.
# Depends on: startup.R (df, rank_lookup, h2h_rates, fatigue_scores, serve_cumulative)

# ── Name utilities ─────────────────────────────────────────────────────────────

normalize_name <- function(x) {
  stri_trans_general(x, "Latin-ASCII") %>% str_trim()
}

abbreviate_name <- function(full_name) {
  parts <- str_split(full_name, " ")[[1]]
  if (length(parts) < 2) return(full_name)
  surname <- parts[length(parts)]
  initial <- paste0(substr(parts[1], 1, 1), ".")
  paste(surname, initial)
}

# ── Form features ──────────────────────────────────────────────────────────────

# Raw win rate over last n matches (all surfaces)
get_recent_form <- function(player_name, match_idx, data, n = 10) {
  past <- data %>%
    filter(match_id < match_idx) %>%
    filter(playerA == player_name | playerB == player_name) %>%
    tail(n) %>%
    mutate(won = if_else(
      playerA == player_name,
      as.numeric(as.character(outcome)) == 1,
      as.numeric(as.character(outcome)) == 0
    ))
  if (nrow(past) == 0) return(NA_real_)
  mean(past$won)
}

# Weighted form: weights wins by 1/opponent_rank (beating top players counts more)
get_weighted_form <- function(player_name, match_idx, data, n = 10) {
  past <- data %>%
    filter(match_id < match_idx) %>%
    filter(playerA == player_name | playerB == player_name) %>%
    tail(n) %>%
    mutate(
      won      = if_else(playerA == player_name,
                         as.numeric(as.character(outcome)) == 1,
                         as.numeric(as.character(outcome)) == 0),
      opp_rank = if_else(playerA == player_name, rankB, rankA),
      weight   = 1 / opp_rank
    )
  if (nrow(past) == 0) return(NA_real_)
  weighted.mean(past$won, past$weight)
}

# Weighted form v2: handles NA ranks with a default weight of 0.01
get_weighted_form_v2 <- function(player_name, match_idx, data, n = 10) {
  past <- data %>%
    filter(match_id < match_idx) %>%
    filter(playerA == player_name | playerB == player_name) %>%
    tail(n) %>%
    mutate(
      won      = if_else(playerA == player_name,
                         as.numeric(as.character(outcome)) == 1,
                         as.numeric(as.character(outcome)) == 0),
      opp_rank = if_else(playerA == player_name, rankB, rankA),
      weight   = if_else(!is.na(opp_rank), 1 / opp_rank, 0.01)
    )
  if (nrow(past) == 0) return(NA_real_)
  weighted.mean(past$won, past$weight)
}

# Surface-specific win rate over last n surface matches
get_surface_winrate <- function(player_name, match_idx, surface_type, data, n = 20) {
  past <- data %>%
    filter(match_id < match_idx) %>%
    filter(playerA == player_name | playerB == player_name) %>%
    filter(surface == surface_type) %>%
    tail(n) %>%
    mutate(won = if_else(
      playerA == player_name,
      as.numeric(as.character(outcome)) == 1,
      as.numeric(as.character(outcome)) == 0
    ))
  if (nrow(past) == 0) return(NA_real_)
  mean(past$won)
}

# ── Rank trajectory ────────────────────────────────────────────────────────────

# Positive value = improving (rank number decreased); negative = declining
get_rank_trajectory <- function(player, match_date, df, days = 90) {
  match_date_proper <- as.Date(as.character(match_date), format = "%Y%m%d")
  cutoff <- match_date_proper - days

  prev_matches <- df %>%
    mutate(date_proper = as.Date(as.character(tourney_date), format = "%Y%m%d")) %>%
    filter(date_proper >= cutoff, date_proper < match_date_proper) %>%
    filter(winner_name == player | loser_name == player) %>%
    mutate(rank = if_else(winner_name == player, winner_rank, loser_rank)) %>%
    arrange(date_proper)

  if (nrow(prev_matches) < 2) return(NA_real_)
  earliest_rank <- first(prev_matches$rank)
  latest_rank   <- last(prev_matches$rank)
  earliest_rank - latest_rank   # positive = rank improved
}

# ── Score parsing ──────────────────────────────────────────────────────────────

parse_score <- function(score) {
  empty <- list(sets_won = NA, sets_lost = NA, games_won = NA,
                games_lost = NA, straight_sets = NA)

  if (is.na(score) || score == "" ||
      grepl("W/O|RET|DEF|Walkover", score, ignore.case = TRUE)) {
    return(empty)
  }

  score_clean <- gsub("\\(\\d+\\)", "", score)
  sets        <- str_split(score_clean, " ")[[1]]
  sets        <- sets[grepl("^\\d+-\\d+$", sets)]

  if (length(sets) == 0) return(empty)

  games      <- str_split(sets, "-")
  winner_g   <- sum(map_dbl(games, ~ as.numeric(.x[1])))
  loser_g    <- sum(map_dbl(games, ~ as.numeric(.x[2])))
  sets_won   <- sum(map_dbl(games, ~ as.numeric(.x[1]) > as.numeric(.x[2])))
  sets_lost  <- sum(map_dbl(games, ~ as.numeric(.x[1]) < as.numeric(.x[2])))

  list(
    sets_won    = sets_won,
    sets_lost   = sets_lost,
    games_won   = winner_g,
    games_lost  = loser_g,
    straight_sets = as.integer(sets_lost == 0)
  )
}

# ── Tournament momentum (historical data) ─────────────────────────────────────

# Returns list(ss_streak, games_dom, avg_minutes) within a tournament up to match m_num
get_tournament_momentum <- function(player, t_id, m_num, df) {
  all_prev <- df %>%
    filter(tourney_id == t_id, match_num < m_num) %>%
    filter(winner_name == player | loser_name == player) %>%
    mutate(
      won          = winner_name == player,
      p_games_won  = if_else(won, games_won,  games_lost),
      p_games_lost = if_else(won, games_lost, games_won)
    )

  if (nrow(all_prev) == 0) {
    return(list(ss_streak = 0, games_dom = NA_real_, avg_minutes = NA_real_))
  }

  # Straight-set streak: count backwards from most recent win
  won_prev <- all_prev %>% filter(won) %>% arrange(match_num)
  ss_streak <- 0
  if (nrow(won_prev) > 0) {
    for (i in nrow(won_prev):1) {
      if (!is.na(won_prev$straight_sets[i]) && won_prev$straight_sets[i] == 1) {
        ss_streak <- ss_streak + 1
      } else break
    }
  }

  total_gw  <- sum(all_prev$p_games_won,  na.rm = TRUE)
  total_gl  <- sum(all_prev$p_games_lost, na.rm = TRUE)
  games_dom <- if_else(total_gw + total_gl > 0,
                       total_gw / (total_gw + total_gl), NA_real_)
  avg_min   <- mean(all_prev$minutes, na.rm = TRUE)

  list(ss_streak = ss_streak, games_dom = games_dom, avg_minutes = avg_min)
}

# ── Tournament serve stats (historical data) ───────────────────────────────────

get_tournament_serve <- function(player, t_id, m_num, df) {
  prev <- df %>%
    filter(tourney_id == t_id, match_num < m_num) %>%
    mutate(won = winner_name == player, lost = loser_name == player) %>%
    filter(won | lost) %>%
    mutate(
      first_in  = if_else(won, w_1stIn,  l_1stIn),
      first_won = if_else(won, w_1stWon, l_1stWon),
      svpt      = if_else(won, w_svpt,   l_svpt)
    )

  if (nrow(prev) == 0 || sum(prev$svpt, na.rm = TRUE) == 0) {
    return(list(serve_win = NA_real_, first_in_pct = NA_real_))
  }

  total_first_in  <- sum(prev$first_in,  na.rm = TRUE)
  total_first_won <- sum(prev$first_won, na.rm = TRUE)
  total_svpt      <- sum(prev$svpt,      na.rm = TRUE)

  list(
    serve_win    = total_first_won / total_first_in,
    first_in_pct = total_first_in  / total_svpt
  )
}

# ── Live tournament stats (in-progress tournament) ────────────────────────────

get_live_games_dom <- function(player, tournament_matches) {
  matches <- tournament_matches %>%
    filter(winner == player | loser == player) %>%
    mutate(
      won = winner == player,
      parsed = map(score, parse_score),
      gw = map2_dbl(parsed, won, ~ if (.y) .x$games_won  else .x$games_lost),
      gl = map2_dbl(parsed, won, ~ if (.y) .x$games_lost else .x$games_won)
    )

  if (nrow(matches) == 0) return(NA_real_)
  total_gw <- sum(matches$gw, na.rm = TRUE)
  total_gl <- sum(matches$gl, na.rm = TRUE)
  total_gw / (total_gw + total_gl)
}

get_games_dom_diff <- function(playerA, playerB, tournament_matches) {
  domA <- get_live_games_dom(playerA, tournament_matches)
  domB <- get_live_games_dom(playerB, tournament_matches)
  if (is.na(domA) || is.na(domB)) return(NA_real_)
  domA - domB
}

get_player_tournament_stats <- function(player, tournament_matches) {
  matches <- tournament_matches %>%
    filter(winner == player | loser == player) %>%
    mutate(
      won    = winner == player,
      parsed = map(score, parse_score),
      gw     = map2_dbl(parsed, won, ~ if (.y) .x$games_won  else .x$games_lost),
      gl     = map2_dbl(parsed, won, ~ if (.y) .x$games_lost else .x$games_won),
      straight = map2_dbl(parsed, won, ~ if (.y) .x$straight_sets else NA_real_)
    )

  if (nrow(matches) == 0) {
    return(list(games_dom = NA_real_, ss_streak = 0, matches_played = 0))
  }

  total_gw  <- sum(matches$gw, na.rm = TRUE)
  total_gl  <- sum(matches$gl, na.rm = TRUE)
  games_dom <- if_else(total_gw + total_gl > 0,
                       total_gw / (total_gw + total_gl), NA_real_)

  won_matches <- matches %>% filter(won) %>% arrange(row_number())
  ss_streak <- 0
  if (nrow(won_matches) > 0) {
    for (i in nrow(won_matches):1) {
      if (!is.na(won_matches$straight[i]) && won_matches$straight[i] == 1) {
        ss_streak <- ss_streak + 1
      } else break
    }
  }

  list(games_dom = games_dom, ss_streak = ss_streak,
       matches_played = nrow(matches))
}

# ── Full feature pipeline (builds df_features1 → df_features6) ───────────────

build_features <- function(raw_df) {
  message("Step 1/6: base + recent form...")
  df1 <- raw_df %>%
    mutate(
      formA     = map2_dbl(playerA, match_id, ~ get_recent_form(.x, .y, raw_df)),
      formB     = map2_dbl(playerB, match_id, ~ get_recent_form(.x, .y, raw_df)),
      form_diff = formA - formB
    )

  message("Step 2/6: weighted form...")
  df2 <- df1 %>%
    mutate(
      wformA     = map2_dbl(playerA, match_id, ~ get_weighted_form_v2(.x, .y, raw_df)),
      wformB     = map2_dbl(playerB, match_id, ~ get_weighted_form_v2(.x, .y, raw_df)),
      wform_diff = wformA - wformB
    )

  message("Step 3/6: surface win rate...")
  df3 <- df2 %>%
    mutate(
      surf_wrA     = pmap_dbl(list(playerA, match_id, surface),
                               ~ get_surface_winrate(..1, ..2, ..3, raw_df)),
      surf_wrB     = pmap_dbl(list(playerB, match_id, surface),
                               ~ get_surface_winrate(..1, ..2, ..3, raw_df)),
      surf_wr_diff = surf_wrA - surf_wrB
    )

  message("Step 4/6: H2H...")
  df4 <- df3 %>%
    left_join(h2h_rates, by = c("playerA", "playerB")) %>%
    mutate(
      h2h_diff_filled      = coalesce(h2h_diff, 0),
      h2h_diff_conditional = if_else(h2h_count >= 3, h2h_diff, 0),
      h2h_v3               = case_when(
        h2h_count >= 5  ~ h2h_diff,
        h2h_count >= 3  ~ h2h_diff * 0.5,
        TRUE            ~ 0
      )
    )

  message("Step 5/6: fatigue...")
  df5 <- df4 %>%
    left_join(fatigue_scores %>% rename(fatigueA = fatigue),
              by = c("playerA" = "player", "tourney_date", "tourney_id")) %>%
    left_join(fatigue_scores %>% rename(fatigueB = fatigue),
              by = c("playerB" = "player", "tourney_date", "tourney_id")) %>%
    mutate(fatigue_diff = fatigueA - fatigueB)

  message("Step 6/6: tournament momentum (ss_streak, games_dom, avg_min)...")
  df6 <- df5 %>%
    mutate(
      log_rank_ratio = log(rankB / rankA)
    )

  message("Feature build complete. Rows: ", nrow(df6), " Cols: ", ncol(df6))
  df6
}
