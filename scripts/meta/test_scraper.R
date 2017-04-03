### TEST SCRAPER ###
# Last edit: Manny (2017-04-01)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC); require(Kmisc)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")
load("~/Documents/github/corsica/modules/stats.RData")


## Test Scraper
# Compile games
game_list <- ds.compile_games(games = 20375:20375,
                              season = "20142015",
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


# Compute goalie stats


