### TEST SCRAPER ###
# Last edit: Manny (2017-03-30)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC); require(Kmisc)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")
load("~/Documents/github/corsica/modules/stats.RData")


## Test Scraper
# Compile games
game_list <- ds.compile_games(games = 20001:20012,
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
    group_by(season, session, game_id, game_date, home_team, game_strength_state) %>%
    st.sum_team("Home"),
  
  pbp %>%
    group_by(season, session, game_id, game_date, away_team, game_strength_state) %>%
    st.sum_team("Away")
) %>%
  data.frame() ->
  team_stats

# Compute skater stats


# Compute goalie stats


