### TEST SCRAPER ###
# Last edit: Manny (2017-04-01)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC); require(Kmisc); require(RSQLite)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")
load("~/Documents/github/corsica/modules/stats.RData")
load("~/Documents/github/corsica/models/xg_model.RData")
load("~/Documents/github/corsica/models/adjustments_model.RData")


## Test Scraper
# Compile games
game_list <- ds.compile_games(games = 30225:30225,
                              season = "20162017",
                              try_tolerance = 3,
                              agents = ds.user_agents
                              )

pbp <- game_list[[1]]

# Enhance PBP
pbp <- st.pbp_enhance(pbp)

# Compute team stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_team, game_strength_state) %>%
    st.sum_team("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_team, game_strength_state) %>%
    st.sum_team("Away")
) %>%
  data.frame() ->
  team_stats_full

team_stats_full %>%
  group_by(team, season, session) %>%
  summarise(GP = length(unique(game_id)),
            TOI = sum(na.omit(TOI)),
            GF = sum(GF),
            GA = sum(GA),
            SF = sum(SF),
            SA = sum(SA),
            PENT2 = sum(PENT2),
            PENT5 = sum(PENT5),
            FOW = sum(FOW),
            FOL = sum(FOL)
            ) %>%
  data.frame() ->
  team_stats_all

# Compute skater stats
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
  skater_stats_full

skater_stats_full %>%
  group_by(player, season, session) %>%
  summarise(GP = length(unique(game_id)),
            TOI = sum(na.omit(TOI)),
            GF = sum(GF),
            GA = sum(GA),
            CF = sum(CF),
            CA = sum(CA),
            iPENT2 = sum(iPENT2),
            iPENT5 = sum(iPENT5),
            iFOW = sum(iFOW),
            iFOL = sum(iFOL),
            G = sum(G),
            A1 = sum(A1),
            A2 = sum(A2),
            iCF = sum(iCF),
            iFF = sum(iFF)
            ) %>%
  data.frame() ->
  skater_stats_all

# Compute goalie stats


