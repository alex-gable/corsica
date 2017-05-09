### STATS ###
# Last edit: Manny (2017-03-30)


# Game Score Formula: 0.750*G + 0.700*A1 + 0.550*A2 + 0.075*iSF + 0.050*iBLK + 0.150*iPEND + 0.010*iFOW + 0.050*CF + 0.150*GF - 0.150*iPENT - 0.010*iFOL - 0.050*CA - 0.150*GA

## Full Team Stats
{
bind_rows(
  pbp %>%
    group_by(season, session, game_id, game_date, home_team, game_score_state, game_strength_state) %>%
    st.sum_team("Home"),
  
  pbp %>%
    group_by(season, session, game_id, game_date, away_team, game_score_state, game_strength_state) %>%
    st.sum_team("Away")
) %>%
  data.frame() ->
  team_stats
}

## Full Skater Stats
{
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, game_strength_state) %>%
    rename(player = home_on_1) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, game_strength_state) %>%
    rename(player = home_on_2) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, game_strength_state) %>%
    rename(player = home_on_3) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_4, game_strength_state) %>%
    rename(player = home_on_4) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_5, game_strength_state) %>%
    rename(player = home_on_5) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_6, game_strength_state) %>%
    rename(player = home_on_6) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, game_strength_state) %>%
    rename(player = away_on_1) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, game_strength_state) %>%
    rename(player = away_on_2) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, game_strength_state) %>%
    rename(player = away_on_3) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_4, game_strength_state) %>%
    rename(player = away_on_4) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_5, game_strength_state) %>%
    rename(player = away_on_5) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_6, game_strength_state) %>%
    rename(player = away_on_6) %>%
    st.sum_skater("Away")
) %>%
  data.frame() ->
  skater_stats
}

## Full Goalie Stats
{
bind_rows(
  pbp %>%
    group_by(season, session, game_id, game_date, home_goalie, game_score_state, game_strength_state) %>%
    st.sum_goalie("Home"),
  
  pbp %>%
    group_by(season, session, game_id, game_date, away_goalie, game_score_state, game_strength_state) %>%
    st.sum_goalie("Away")
) %>%
  data.frame() ->
  goalie_stats
}
