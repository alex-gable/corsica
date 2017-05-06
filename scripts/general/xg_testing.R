### xG TESTING ###
# Last edit: Manny (2017-04-27)

## Dependencies
require(dplyr); require(RSQLite); require(doMC); require(neuralnet); require(glmnet)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/stats.RData")


## Functions
# Meta

# General


## Load data
# Connect to database
conn <- dbConnect(SQLite(), "~/Documents/corsica_data/raw.sqlite")

# Read table
pbp <- dbReadTable(conn, "pbp")

# Disconnect
dbDisconnect(conn)


## Prepare data
# Enhance PBP
pbp <- st.pbp_enhance(pbp)

# Add additional features
pbp %>%
  filter(event_type %in% c("FACEOFF",
                           "TAKEAWAY", 
                           "BLOCKED_SHOT", 
                           "SHOT", 
                           "MISSED_SHOT", 
                           "HIT", 
                           "GIVEAWAY"
                           )
         ) %>%
  group_by(season, game_id) %>%
  arrange(event_index) %>%
  mutate(seconds_since_last = game_seconds - lag(game_seconds, 1),
         event_type_last = lag(event_type, 1),
         event_team_last = lag(event_team, 1),
         event_rinkside_last = lag(event_rinkside, 1),
         coords_x_last = lag(coords_x, 1),
         coords_y_last = lag(coords_y, 1)
         ) %>%
  ungroup() %>%
  arrange(season, game_id, event_index) %>%
  select(season,
         session,
         game_id,
         game_date,
         game_period,
         game_seconds,
         game_strength_state,
         game_score_state,
         home_goalie,
         away_goalie,
         home_score,
         away_score,
         home_skaters,
         away_skaters,
         home_team,
         away_team,
         event_type,
         event_detail,
         event_distance,
         event_angle,
         event_rinkside,
         event_team,
         coords_x,
         coords_y,
         seconds_since_last,
         event_type_last,
         event_team_last,
         event_rinkside_last,
         coords_x_last,
         coords_y_last
         ) %>%
  data.frame() ->
  pbp

# Select model data
pbp %>%
  filter(event_type %in% c("GOAL", 
                           "SHOT", 
                           "MISSED_SHOT"
                           ),
         !{game_period > 4 & session == "R"},
         !is.na(coords_x),
         !is.na(coords_y),
         game_strength_state %in% c("5v5",
                                    "4v4",
                                    "3v3",
                                    "5v4",
                                    "4v5",
                                    "5v3",
                                    "3v5",
                                    "4v3",
                                    "3v4",
                                    "EvE"
                                    )
         ) %>%
  mutate(same_team_last = 1*(event_team == event_team_last)) %>%
  data.frame() ->
  model_data

# Fix NA
model_data$event_detail[which(is.na(model_data$event_detail) == TRUE)] <- "Unknown"
