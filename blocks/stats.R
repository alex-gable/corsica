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

}

## Full Goalie Stats
{

}