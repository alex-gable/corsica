### xG TRAINING ###
# Last edit: Manny (2017-05-06)

## Dependencies
require(dplyr); require(RSQLite); require(doMC); require(neuralnet); 
require(glmnet); require(Kmisc); require(caret)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/stats.RData")

# load("~/Documents/github/corsica/models/training/xg_training_data.RData")

## Functions
# Meta

# General
ftable2df <- function(mydata) {
  
  ifelse(class(mydata) == "ftable", 
         mydata <- mydata, 
         mydata <- ftable(mydata)
         )
  
  dfrows <- rev(expand.grid(rev(attr(mydata, "row.vars"))))
  
  dfcols <- as.data.frame.matrix(mydata)
  
  do.call(paste, 
          c(rev(expand.grid(rev(attr(mydata, "col.vars")))), 
            sep = "_"
            )
          ) -> names(dfcols)
  
  cbind(dfrows, dfcols)
  
}

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
                           "GIVEAWAY",
                           "GOAL"
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
  filter(event_type %in% st.fenwick_events,
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
                                    "5vE",
                                    "Ev5",
                                    "4vE",
                                    "Ev4",
                                    "3vE",
                                    "Ev3"
                                    )
         ) %>%
  mutate(same_team_last = 1*(event_team == event_team_last),
         is_home_team = 1*(event_team == home_team),
         is_EN = 1*({event_team == home_team & away_goalie == 0} | {event_team == away_team & home_goalie == 0})
         ) %>%
  data.frame() ->
  model_data

# Shooting team reference
model_data$shooter_strength_state <- ifelse(model_data$is_home_team == 1,
                                            model_data$game_strength_state,
                                            str_rev(model_data$game_strength_state)
                                            )

model_data$shooter_score_adv <- ifelse(model_data$is_home_team == 1,
                                       model_data$home_score - model_data$away_score,
                                       model_data$away_score - model_data$home_score
                                       )

# Fix NA
model_data$event_detail[which(is.na(model_data$event_detail) == TRUE)] <- "Unknown"
model_data <- na.omit(model_data)

# Add distance from previous event
model_data$distance_from_last <- sqrt((model_data$coords_x - model_data$coords_x_last)^2 + (model_data$coords_y - model_data$coords_y_last)^2)

# Add board shots
model_data$along_boards_2 <- 1*(abs(model_data$coords_y) > 40)
model_data$along_boards_3 <- 1*(abs(model_data$coords_y) > 39)
model_data$along_boards_4 <- 1*(abs(model_data$coords_y) > 38)
model_data$along_boards_5 <- 1*(abs(model_data$coords_y) > 37)

# Save
save(list = c("pbp", "model_data"), file = "~/Documents/github/corsica/models/training/xg_training_data.RData")


## Model training
# Response variables
model_data$is_goal <- 1*(model_data$event_type == "GOAL")
model_data$is_save <- 1*(model_data$event_type == "SHOT")

# Coerce to factor
model_data$event_type_last <- as.factor(model_data$event_type_last)
model_data$shooter_strength_state <- as.factor(model_data$shooter_strength_state)

# Feature list
vars <- c("event_distance",
          "event_angle",
          "seconds_since_last",
          "event_type_last",
          "same_team_last",
          "is_home_team",
          "is_EN",
          "shooter_strength_state",
          "shooter_score_adv",
          "distance_from_last"
          )

# Build dummy vars
xg_process <- dummyVars(is_goal ~ 
                        poly(event_distance, 3, raw = TRUE) + poly(event_angle, 3, raw = TRUE) + 
                        event_type_last*same_team_last*(seconds_since_last + distance_from_last) + 
                        is_home_team + is_EN + shooter_strength_state + shooter_score_adv,
                        data = model_data[, c("is_goal", vars)],
                        contrasts = TRUE,
                        fullRank = FALSE
                        )

# Build model matrix
model_mat <- cbind(outcome = as.factor(1*(model_data$is_save) + 2*(model_data$is_goal) + 1),
                   predict(xg_process,
                           model_data[, c("is_goal", vars)]
                           ) %>%
                     data.frame()
                   )

# Fit GLM
xg_glm <- cv.glmnet(x = as.matrix(model_mat[, -1]),
                    y = as.factor(model_mat[, 1]),
                    family = "multinomial",
                    nfolds = 4,
                    nlambda = 100,
                    alpha = 1
                    )

# Save
save(list = c("xg_glm", "xg_process"), file = "~/Documents/github/corsica/models/xg_model.RData")

