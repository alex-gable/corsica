### TEST SCRAPER ###
# Last edit: Manny (2017-03-29)

## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")


## Test Scraper
# Compile games
game_list <- ds.compile_games(games = 20001:20012,
                              season = "20162017",
                              try_tolerance = 3,
                              agents = ds.user_agents
                              )

pbp <- game_list[[1]]

# Compute team stats
bind_rows(
  pbp %>%
    group_by(game_id, home_team, game_strength_state) %>%
    rename(team = home_team) %>%
    summarise(CF = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT") & event_team == team),
              CA = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT") & event_team == away_team),
              FF = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT") & event_team == team),
              FA = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT") & event_team == away_team),
              SF = sum(event_type %in% c("SHOT", "GOAL") & event_team == team),
              SA = sum(event_type %in% c("SHOT", "GOAL") & event_team == away_team),
              GF = sum(event_type == "GOAL" & event_team == team),
              GA = sum(event_type == "GOAL" & event_team == away_team),
              TOI = sum(event_length)
              ),
  
  pbp %>%
    group_by(game_id, away_team, game_strength_state) %>%
    rename(team = away_team) %>%
    summarise(CF = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT") & event_team == team),
              CA = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT") & event_team == home_team),
              FF = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT") & event_team == team),
              FA = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT") & event_team == home_team),
              SF = sum(event_type %in% c("SHOT", "GOAL") & event_team == team),
              SA = sum(event_type %in% c("SHOT", "GOAL") & event_team == home_team),
              GF = sum(event_type == "GOAL" & event_team == team),
              GA = sum(event_type == "GOAL" & event_team == home_team),
              TOI = sum(event_length)
              )
) %>%
  data.frame() ->
  team_stats

# Compute skater stats


# Compute goalie stats


