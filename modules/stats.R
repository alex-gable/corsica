### STATS ###
# Last edit: Manny (2017-05-06)


## Description
# Stats contains all functions and tools related to compiling stats from raw data for Corsica 
# Dependencies: dplyr, Kmisc, doMC, user_functions


## Dependencies
require(dplyr); require(Kmisc); require(doMC); require(glmnet); require(survival)


## Objects
c("G" = 0.75,
  "A1" = 0.7,
  "A2" = 0.55,
  "iSF" = 0.075,
  "iBLK" = 0.05,
  "iPENT" = -0.15,
  "iPEND" = 0.15,
  "iFOW" = 0.01,
  "iFOL" = -0.01,
  "CF" = 0.05,
  "CA" = -0.05,
  "GF" = 0.15,
  "GA" = -0.15
  ) ->
  st.game_score_weights

c("SHOT", 
  "GOAL"
  ) ->
  st.shot_events

c("SHOT", 
  "GOAL",
  "MISSED_SHOT"
  ) ->
  st.fenwick_events

c("SHOT", 
  "GOAL",
  "MISSED_SHOT",
  "BLOCKED_SHOT"
  ) ->
  st.corsi_events

c("3v3",
  "5v5", 
  "4v4", 
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
  ) %>%
  as.factor() ->
  st.strength_states

## Meta Functions
# GSAA
st.gsaa <- function() {}     # TO BE ADDED

# Game Score
st.game_score <- function(x) {
  
  ## Description
  # game_score() returns the game score obtained from a given vector of statistics
  # The vector x is expected to contain the necessary stats in proper order
  
  return(sum(st.game_score_weights*x))
  
}

# Distance from Net
st.distance_from_net <- function(x, y) {
  
  ## Description
  # distance_from_net() returns the distance from the nearest net in feet of a location corresponding \
  # to a given set of coordinates
  
  return(sqrt((89 - abs(nabs(x)))^2 + nabs(y)^2))
  
}

# Angle from Centre
st.angle_from_centre <- function(x, y) {
  
  ## Description
  # angle_from_centre() returns the angle from the central line perpendicular to the goal line in \
  # degrees of a location corresponsing to a given set of coordinates
  
  return(abs(atan(nabs(y)/(89 - abs(nabs(x))))*(180/pi)))
  
}

# Which Zone
st.which_zone <- function(x) {
  
  ## Description
  # which_zone() returns the absolute zone of a location corresponding to a given x-coordinate
  
  factor_level <- as.factor(1*(x <= -25) +
                            2*(abs(nabs(x)) < 25) +
                            3*(x >= 25)
                            )
  
  levels(factor_level) <- c("L",
                            "N",
                            "R"
                            )
  
  return(as.character(factor_level))
  
}

st.which_circle <- function(x, y) {
  
  ## Description
  # which_circle() returns the faceoff circle number nearest to a location corresponding to a given \
  # set of coordinates
  
  circle <- 1*(nabs(x) <= -25 & nabs(y) > 0) +
            2*(nabs(x) <= -25 & nabs(y) < 0) +
            3*(nabs(x) < 0 & nabs(x) > 25 & nabs(y) > 0) +
            4*(nabs(x) < 0 & nabs(x) > 25 & nabs(y) < 0) +
            5*(abs(nabs(x)) < 5 & abs(nabs(y)) < 5) +
            6*(nabs(x) > 0 & nabs(x) < 25 & nabs(y) > 0) +
            7*(nabs(x) > 0 & nabs(x) < 25 & nabs(y) < 0) +
            8*(nabs(x) >= 25 & nabs(y) > 0) +
            9*(nabs(x) >= 25 & nabs(y) < 0)
  
  return(circle)
  
}


## General Functions
# Enhance PBP
st.pbp_enhance <- function(pbp) {
  
  ## Description
  # pbp_enhance() performs some preliminary operations on a given PBP data frame object and returns \
  # the enhanced version
  
  pbp %>%
    mutate(event_distance = st.distance_from_net(coords_x, coords_y),
           event_angle = st.angle_from_centre(coords_x, coords_y),
           event_rinkside = st.which_zone(coords_x),
           event_circle = st.which_circle(coords_x, coords_y)
           ) %>%
    data.frame() ->
    enhanced_pbp
  
  enhanced_pbp %>%
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
         event_index,
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
  xg_pbp

  xg_pbp$game_strength_state[which(xg_pbp$game_strength_state %in% st.strength_states == FALSE)] <- "5v5"
  
  xg_pbp %>%
  filter(event_type %in% st.fenwick_events,
         !{game_period > 4 & session == "R"},
         !is.na(coords_x),
         !is.na(coords_y)
         ) %>%
  mutate(same_team_last = 1*(event_team == event_team_last),
         is_home_team = 1*(event_team == home_team),
         is_EN = 1*({event_team == home_team & away_goalie == 0} | {event_team == away_team & home_goalie == 0})
         ) %>%
  select(-c(home_goalie, away_goalie)) %>%
  data.frame() ->
  model_data
  
  model_data$shooter_strength_state <- ifelse(model_data$is_home_team == 1,
                                              model_data$game_strength_state,
                                              str_rev(model_data$game_strength_state)
                                              )

  model_data$shooter_score_adv <- ifelse(model_data$is_home_team == 1,
                                         model_data$home_score - model_data$away_score,
                                         model_data$away_score - model_data$home_score
                                         )
  
  model_data$event_detail <- as.character(model_data$event_detail)
  model_data$event_detail[which(is.na(model_data$event_detail) == TRUE)] <- "Unknown"
  model_data <- na.omit(model_data)
  
  model_data$distance_from_last <- sqrt((model_data$coords_x - model_data$coords_x_last)^2 + (model_data$coords_y - model_data$coords_y_last)^2)

  model_data$is_goal <- 1*(model_data$event_type == "GOAL")
  model_data$is_save <- 1*(model_data$event_type == "SHOT")
  
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
  
  model_data$event_type_last <- as.factor(model_data$event_type_last)
  model_data$shooter_strength_state <- as.factor(model_data$shooter_strength_state)
  
  model_mat <- cbind(outcome = as.factor(1*(model_data$is_save) + 2*(model_data$is_goal) + 1),
                     predict(xg_process,
                             model_data[, c("is_goal", vars)]
                             ) %>%
                       data.frame()
                     )
  
  predicted <- glmnet::predict.cv.glmnet(xg_glm,
                                         newx = as.matrix(model_mat[, -1]),
                                         s = "lambda.min",
                                         type = "response"
                                         )
  
  ftable2df(predicted) %>%
    rename(val = `1`) %>%
    group_by(Var2) %>%
    summarise(prob_miss = sum(val*(Var1 == 1)),
              prob_save = sum(val*(Var1 == 2)),
              prob_goal = sum(val*(Var1 == 3))
              ) %>%
    data.frame() ->
    prob_df
  
  model_data$prob_goal <- as.numeric(prob_df$prob_goal)
  model_data$prob_save <- as.numeric(prob_df$prob_save)
  
  merge(enhanced_pbp,
        model_data %>%
          select(game_id, event_index, prob_goal, prob_save) %>%
          data.frame(),
        by.x = c("game_id", "event_index"),
        by.y = c("game_id", "event_index"),
        all.x = TRUE
        ) %>%
    data.frame() ->
    enhanced_pbp
  
  enhanced_pbp %>%
  filter(event_type %in% c(st.corsi_events, "FACEOFF"),
         !{session == "R" & game_period > 4},
         !is.na(home_rinkside),
         !is.na(away_rinkside)
         ) %>%
  arrange(game_id, event_index) %>%
  mutate(faceoff_ref = cumsum(event_type == "FACEOFF")) %>%
  group_by(game_id, faceoff_ref) %>%
  arrange(event_index) %>%
  mutate(seconds_since = nabs(game_seconds) - min(nabs(game_seconds)),
         rinkside_start = first(event_rinkside),
         is_home_team = 1*({event_type %in% st.fenwick_events & event_team == home_team} |
                           {event_type == "BLOCKED_SHOT" & event_team == away_team}),
         home_zonestart = 1*(rinkside_start == home_rinkside) + 2*(rinkside_start == "N") + 3*(rinkside_start != home_rinkside & rinkside_start != "N"),
         home_score_adv = home_score - away_score
         ) %>%
  ungroup() %>%
  arrange(game_id, event_index) %>%
  data.frame() ->
  adj_pbp
  
  adj_pbp$game_strength_state[which(adj_pbp$game_strength_state %in% st.strength_states == FALSE)] <- "5v5"
  
  adj_pbp %>%
  filter(game_strength_state %in% st.strength_states) %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(elapsed = game_seconds - lag(game_seconds, 1),
         hazard_goal = 1*(event_type == "GOAL"),
         hazard_shot = 1*(event_type %in% st.shot_events),
         hazard_fenwick = 1*(event_type %in% st.fenwick_events),
         hazard_corsi = 1*(event_type %in% st.corsi_events)
         ) %>%
  ungroup() %>%
  select(game_id,
         event_index,
         event_type,
         elapsed,
         hazard_goal:hazard_corsi,
         is_home_team,
         seconds_since,
         rinkside_start,
         home_zonestart,
         game_strength_state,
         home_score_adv,
         game_seconds
         ) %>%
  data.frame() ->
  model_data

  model_data$elapsed[which(is.na(model_data$elapsed) == TRUE)] <- 0
  model_data$elapsed[which(model_data$elapsed == 0)] <- 0.01

  model_data$home_zonestart <- as.factor(model_data$home_zonestart)
  model_data$game_strength_state <- as.factor(model_data$game_strength_state)
  
  model_mat <- predict(home_corsi_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_home_corsi <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_corsi, 
                                                                          as.matrix(model_mat), 
                                                                          s = "lambda.min"
                                                                          )
                                                )
                                     )
  
  model_mat <- predict(home_fenwick_process,
                       model_data
                       ) %>%
                 data.frame()

  model_data$adj_home_fenwick <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_fenwick, 
                                                                            as.matrix(model_mat), 
                                                                            s = "lambda.min"
                                                                            )
                                                  )
                                       )  

  model_mat <- predict(home_shot_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_home_shot <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_shot, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  model_mat <- predict(home_goal_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_home_goal <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_goal, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  model_mat <- predict(away_corsi_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_corsi <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_corsi, 
                                                                          as.matrix(model_mat), 
                                                                          s = "lambda.min"
                                                                          )
                                                )
                                     )
  
  model_mat <- predict(away_fenwick_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_fenwick <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_fenwick, 
                                                                            as.matrix(model_mat), 
                                                                            s = "lambda.min"
                                                                            )
                                                  )
                                       )
  
  model_mat <- predict(away_shot_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_shot <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_shot, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  model_mat <- predict(away_goal_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_goal <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_goal, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  merge(enhanced_pbp,
        model_data %>%
          select(game_id, event_index, adj_home_corsi:adj_away_goal) %>%
          data.frame(),
        by.x = c("game_id", "event_index"),
        by.y = c("game_id", "event_index"),
        all.x = TRUE
        ) %>%
    data.frame() ->
    enhanced_pbp
  
  return(enhanced_pbp)
  
}

# Summarize Team Stats
st.sum_team <- function(x, venue) {
  
  ## Description
  # sum_team() summarizes all team counting stats from a PBP data frame object
  # x is expected to be a grouped data frame with home_team or away_team as a grouping variable for \
  # venue = "home" and venue = "away" respectively
  
  venue_ <- tolower(as.character(venue))
  
  if(venue_ == "home") {
    
    x %>%
      rename(team = home_team) %>%
      summarise(venue = "Home",
                GP = length(unique(game_id)),
                TOI = sum(event_length)/60,
                CF = sum({event_type %in% st.fenwick_events & event_team == team} |
                         {event_type == "BLOCKED_SHOT" & event_team == away_team}
                         ),
                CA = sum({event_type %in% st.fenwick_events & event_team == away_team} |
                         {event_type == "BLOCKED_SHOT" & event_team == team}
                         ),
                FF = sum(event_type %in% st.fenwick_events & event_team == team),
                FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                SF = sum(event_type %in% st.shot_events & event_team == team),
                SA = sum(event_type %in% st.shot_events & event_team == away_team),
                GF = sum(event_type == "GOAL" & event_team == team),
                GA = sum(event_type == "GOAL" & event_team == away_team),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                FOW = sum(event_type == "FACEOFF" & event_team == team),
                FOL = sum(event_type == "FACEOFF" & event_team == away_team),
                PENT2 = sum(1*(event_type == "PENALTY" & event_team == team) +
                            1*(event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENALTY" & event_team == team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PENTS = sum(event_type == "PENALTY" & event_team == team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                PEND2 = sum(1*(event_type == "PENALTY" & event_team == away_team) +
                            1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PENDS = sum(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                
                GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                HF = sum(event_type == "HIT" & event_team == team),
                HA = sum(event_type == "HIT" & event_team == away_team)
                ) %>%
      data.frame() %>%
      return()
    
  } else if(venue_ == "away") {
    
    x %>%
      rename(team = away_team) %>%
      summarise(venue = "Away",
                GP = length(unique(game_id)),
                TOI = sum(event_length)/60,
                CF = sum({event_type %in% st.fenwick_events & event_team == team} |
                         {event_type == "BLOCKED_SHOT" & event_team == home_team}
                         ),
                CA = sum({event_type %in% st.fenwick_events & event_team == home_team} |
                         {event_type == "BLOCKED_SHOT" & event_team == team}
                         ),
                FF = sum(event_type %in% st.fenwick_events & event_team == team),
                FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                SF = sum(event_type %in% st.shot_events & event_team == team),
                SA = sum(event_type %in% st.shot_events & event_team == home_team),
                GF = sum(event_type == "GOAL" & event_team == team),
                GA = sum(event_type == "GOAL" & event_team == home_team),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                FOW = sum(event_type == "FACEOFF" & event_team == team),
                FOL = sum(event_type == "FACEOFF" & event_team == home_team),
                PENT2 = sum(1*(event_type == "PENALTY" & event_team == team) +
                            1*(event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENALTY" & event_team == team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PENTS = sum(event_type == "PENALTY" & event_team == team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                PEND2 = sum(1*(event_type == "PENALTY" & event_team == home_team) +
                            1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PENDS = sum(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                HF = sum(event_type == "HIT" & event_team == team),
                HA = sum(event_type == "HIT" & event_team == home_team)
                ) %>%
      data.frame() %>%
      return()
    
  }
  
}

# Summarize Skater Stats
st.sum_skater <- function(x, venue) {
  
  ## Description
  # sum_skater() summarizes all skater counting stats from a PBP data frame object
  # x is expected to be a grouped data frame with home_on_x or away_on_x as a grouping variable \
  # for venue = "home" and venue = "away" respectively
  # A rename() argument must be passed before sum_skater() to convert home/away_on_x to player
  
  venue_ <- tolower(as.character(venue))
  
  if(venue_ == "home") {
    
    x %>%
      summarise(venue = "Home",
                GP = length(unique(game_id)),
                TOI = sum(event_length)/60,
                CF = sum({event_type %in% st.fenwick_events & event_team == home_team} |
                         {event_type == "BLOCKED_SHOT" & event_team == away_team}
                         ),
                CA = sum({event_type %in% st.fenwick_events & event_team == away_team} |
                         {event_type == "BLOCKED_SHOT" & event_team == home_team}
                         ),
                FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
                FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                SF = sum(event_type %in% st.shot_events & event_team == home_team),
                SA = sum(event_type %in% st.shot_events & event_team == away_team),
                GF = sum(event_type == "GOAL" & event_team == home_team),
                GA = sum(event_type == "GOAL" & event_team == away_team),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                PENT2 = sum(1*(event_type == "PENALTY" & event_team == home_team) +
                            1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PEND2 = sum(1*(event_type == "PENALTY" & event_team == away_team) +
                            1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                
                iCF = sum({event_type %in% st.fenwick_events & event_player_1 == player} |
                          {event_type == "BLOCKED_SHOT" & event_player_2 == player}
                          ),
                iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player),
                iSF = sum(event_type %in% st.shot_events & event_player_1 == player),
                G = sum(event_type == "GOAL" & event_player_1 == player),
                A1 = sum(na.omit(event_type == "GOAL" & event_player_2 == player)),
                A2 = sum(na.omit(event_type == "GOAL" & event_player_3 == player)),
                iGVA = sum(event_type == "GIVEAWAY" & event_player_1 == player),
                iTKA = sum(event_type == "TAKEAWAY" & event_player_1 == player),
                iHF = sum(event_type == "HIT" & event_player_1 == player),
                iHA = sum(event_type == "HIT" & event_player_2 == player),
                iBLK = sum(event_type == "BLOCKED_SHOT" & event_player_1 == player),
                iFOW = sum(event_type == "FACEOFF" & event_player_1 == player),
                iFOL = sum(event_type == "FACEOFF" & event_player_2 == player),
                iPENT2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                     1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENALTY" & event_player_1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPENT5 = sum(na.omit(event_type == "PENALTY" & event_player_1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                iPEND2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                     1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENALTY" & event_player_2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPEND5 = sum(na.omit(event_type == "PENALTY" & event_player_2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
                ) %>%
      data.frame() %>%
      return()
    
  } else if(venue_ == "away") {
    
    x %>%
      summarise(venue = "Away",
                GP = length(unique(game_id)),
                TOI = sum(event_length)/60,
                CF = sum({event_type %in% st.fenwick_events & event_team == away_team} |
                         {event_type == "BLOCKED_SHOT" & event_team == home_team}
                         ),
                CA = sum({event_type %in% st.fenwick_events & event_team == home_team} |
                         {event_type == "BLOCKED_SHOT" & event_team == away_team}
                         ),
                FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
                FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                SF = sum(event_type %in% st.shot_events & event_team == away_team),
                SA = sum(event_type %in% st.shot_events & event_team == home_team),
                GF = sum(event_type == "GOAL" & event_team == away_team),
                GA = sum(event_type == "GOAL" & event_team == home_team),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != away_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == away_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                PENT2 = sum(1*(event_type == "PENALTY" & event_team == away_team) +
                            1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PEND2 = sum(1*(event_type == "PENALTY" & event_team == home_team) +
                            1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                
                iCF = sum({event_type %in% st.fenwick_events & event_player_1 == player} |
                          {event_type == "BLOCKED_SHOT" & event_player_2 == player}
                          ),
                iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player),
                iSF = sum(event_type %in% st.shot_events & event_player_1 == player),
                G = sum(event_type == "GOAL" & event_player_1 == player),
                A1 = sum(na.omit(event_type == "GOAL" & event_player_2 == player)),
                A2 = sum(na.omit(event_type == "GOAL" & event_player_3 == player)),
                iGVA = sum(event_type == "GIVEAWAY" & event_player_1 == player),
                iTKA = sum(event_type == "TAKEAWAY" & event_player_1 == player),
                iHF = sum(event_type == "HIT" & event_player_1 == player),
                iHA = sum(event_type == "HIT" & event_player_2 == player),
                iBLK = sum(event_type == "BLOCKED_SHOT" & event_player_1 == player),
                iFOW = sum(event_type == "FACEOFF" & event_player_1 == player),
                iFOL = sum(event_type == "FACEOFF" & event_player_2 == player),
                iPENT2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                     1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENALTY" & event_player_1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPENT5 = sum(na.omit(event_type == "PENALTY" & event_player_1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                iPEND2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                     1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENALTY" & event_player_2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPEND5 = sum(na.omit(event_type == "PENALTY" & event_player_2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
                ) %>%
      data.frame() %>%
      return()
    
  }
  
}

# Summarize Team Stats (Old PBP Format)
st.old_sum_team <- function(x, venue) {
  
  ## Description
  # old_sum_team() summarizes all team counting stats from a Corsica 1.0 PBP data frame object
  # x is expected to be a grouped data frame with home_team or away_team as a grouping variable for \
  # venue = "home" and venue = "away" respectively
  
  venue_ <- tolower(as.character(venue))
  
  if(venue_ == "home") {
    
    x %>%
      rename(team = home_team) %>%
      summarise(venue = "Home",
                GP = length(unique(game_id)),
                TOI = sum(nabs(Event.Length))/60,
                CF = sum(event_type %in% st.corsi_events & event_team == team),
                CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                FF = sum(event_type %in% st.fenwick_events & event_team == team),
                FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                SF = sum(event_type %in% st.shot_events & event_team == team),
                SA = sum(event_type %in% st.shot_events & event_team == away_team),
                GF = sum(event_type == "GOAL" & event_team == team),
                GA = sum(event_type == "GOAL" & event_team == away_team),
                xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                ACF = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == team))),
                ACA = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                AFF = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == team))),
                AFA = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                ASF = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == team))),
                ASA = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                AGF = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == team))),
                AGA = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                AxGF = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                AxGA = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                FOW = sum(event_type == "FACEOFF" & event_team == team),
                FOL = sum(event_type == "FACEOFF" & event_team == away_team),
                PENT2 = sum(1*(event_type == "PENL" & event_team == team) +
                            1*(event_type == "PENL" & event_team == team & grepl("double minor", tolower(event_description)) == TRUE) -
                            1*(event_type == "PENL" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENL" & event_team == team & grepl("fighting|major", tolower(event_description)) == TRUE),
                PENTS = sum(event_type == "PENL" & event_team == team & grepl("ps \\-", tolower(event_description)) == TRUE),
                PEND2 = sum(1*(event_type == "PENL" & event_team == away_team) +
                            1*(event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_description)) == TRUE) -
                            1*(event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_description)) == TRUE),
                PENDS = sum(event_type == "PENL" & event_team == away_team & grepl("ps \\-", tolower(event_description)) == TRUE),
                
                GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                HF = sum(event_type == "HIT" & event_team == team),
                HA = sum(event_type == "HIT" & event_team == away_team)
                ) %>%
      data.frame() %>%
      return()
    
  } else if(venue_ == "away") {
    
    x %>%
      rename(team = away_team) %>%
      summarise(venue = "Away",
                GP = length(unique(game_id)),
                TOI = sum(nabs(Event.Length))/60,
                CF = sum(event_type %in% st.corsi_events & event_team == team),
                CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                FF = sum(event_type %in% st.fenwick_events & event_team == team),
                FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                SF = sum(event_type %in% st.shot_events & event_team == team),
                SA = sum(event_type %in% st.shot_events & event_team == home_team),
                GF = sum(event_type == "GOAL" & event_team == team),
                GA = sum(event_type == "GOAL" & event_team == home_team),
                xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                ACF = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == team))),
                ACA = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                AFF = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == team))),
                AFA = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                ASF = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == team))),
                ASA = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                AGF = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == team))),
                AGA = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                AxGF = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                AxGA = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                FOW = sum(event_type == "FACEOFF" & event_team == team),
                FOL = sum(event_type == "FACEOFF" & event_team == home_team),
                PENT2 = sum(1*(event_type == "PENL" & event_team == team) +
                            1*(event_type == "PENL" & event_team == team & grepl("double minor", tolower(event_description)) == TRUE) -
                            1*(event_type == "PENL" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENL" & event_team == team & grepl("fighting|major", tolower(event_description)) == TRUE),
                PENTS = sum(event_type == "PENL" & event_team == team & grepl("ps \\-", tolower(event_description)) == TRUE),
                PEND2 = sum(1*(event_type == "PENL" & event_team == home_team) +
                            1*(event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_description)) == TRUE) -
                            1*(event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_description)) == TRUE),
                PENDS = sum(event_type == "PENL" & event_team == home_team & grepl("ps \\-", tolower(event_description)) == TRUE),
                GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                HF = sum(event_type == "HIT" & event_team == team),
                HA = sum(event_type == "HIT" & event_team == home_team)
                ) %>%
      data.frame() %>%
      return()
    
  }
  
}

# Summarize Skater Stats (Old PBP Format)
st.old_sum_skater <- function(x, venue) {
  
  ## Description
  # old_sum_skater() summarizes all skater counting stats from a Corsica 1.0 PBP data frame object
  # x is expected to be a grouped data frame with home_on_x or away_on_x as a grouping variable \
  # for venue = "home" and venue = "away" respectively
  # A rename() argument must be passed before sum_skater() to convert home/away_on_x to player
  
  venue_ <- tolower(as.character(venue))
  
  if(venue_ == "home") {
    
    x %>%
      summarise(venue = "Home",
                GP = length(unique(game_id)),
                TOI = sum(nabs(Event.Length))/60,
                CF = sum(event_type %in% st.corsi_events & event_team == home_team),
                CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
                FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                SF = sum(event_type %in% st.shot_events & event_team == home_team),
                SA = sum(event_type %in% st.shot_events & event_team == away_team),
                GF = sum(event_type == "GOAL" & event_team == home_team),
                GA = sum(event_type == "GOAL" & event_team == away_team),
                xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                ACF = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                ACA = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                AFF = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                AFA = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                ASF = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                ASA = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                AGF = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                AGA = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                AxGF = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                AxGA = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                PENT2 = sum(1*(event_type == "PENL" & event_team == home_team) +
                            1*(event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PEND2 = sum(1*(event_type == "PENL" & event_team == away_team) +
                            1*(event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                
                iCF = sum(event_type %in% st.corsi_events & p1 == player),
                iFF = sum(event_type %in% st.fenwick_events & p1 == player),
                iSF = sum(event_type %in% st.shot_events & p1 == player),
                ixGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & p1 == player))),
                G = sum(event_type == "GOAL" & p1 == player),
                A1 = sum(na.omit(event_type == "GOAL" & p2 == player)),
                A2 = sum(na.omit(event_type == "GOAL" & p3 == player)),
                iGVA = sum(event_type == "GIVEAWAY" & p1 == player),
                iTKA = sum(event_type == "TAKEAWAY" & p1 == player),
                iHF = sum(event_type == "HIT" & p1 == player),
                iHA = sum(event_type == "HIT" & p2 == player),
                iBLK = sum(event_type == "BLOCKED_SHOT" & p2 == player),
                iFOW = sum(event_type == "FACEOFF" & p1 == player),
                iFOL = sum(event_type == "FACEOFF" & p2 == player),
                iPENT2 = sum(na.omit(1*(event_type == "PENL" & p1 == player) +
                                     1*(event_type == "PENL" & p1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENL" & p1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPENT5 = sum(na.omit(event_type == "PENL" & p1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                iPEND2 = sum(na.omit(1*(event_type == "PENL" & p2 == player) +
                                     1*(event_type == "PENL" & p2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENL" & p2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPEND5 = sum(na.omit(event_type == "PENL" & p2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
                ) %>%
      data.frame() %>%
      return()
    
  } else if(venue_ == "away") {
    
    x %>%
      summarise(venue = "Away",
                GP = length(unique(game_id)),
                TOI = sum(nabs(Event.Length))/60,
                CF = sum(event_type %in% st.corsi_events & event_team == away_team),
                CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
                FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                SF = sum(event_type %in% st.shot_events & event_team == away_team),
                SA = sum(event_type %in% st.shot_events & event_team == home_team),
                GF = sum(event_type == "GOAL" & event_team == away_team),
                GA = sum(event_type == "GOAL" & event_team == home_team),
                xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                ACF = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                ACA = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                AFF = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                AFA = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                ASF = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                ASA = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                AGF = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                AGA = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                AxGF = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                AxGA = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != away_rinkside),
                DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == away_rinkside),
                NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                PENT2 = sum(1*(event_type == "PENL" & event_team == away_team) +
                            1*(event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PENT5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                PEND2 = sum(1*(event_type == "PENL" & event_team == home_team) +
                            1*(event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                            1*(event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                            ),
                PEND5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                
                iCF = sum(event_type %in% st.corsi_events & p1 == player),
                iFF = sum(event_type %in% st.fenwick_events & p1 == player),
                iSF = sum(event_type %in% st.shot_events & p1 == player),
                ixGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & p1 == player))),
                G = sum(event_type == "GOAL" & p1 == player),
                A1 = sum(na.omit(event_type == "GOAL" & p2 == player)),
                A2 = sum(na.omit(event_type == "GOAL" & p3 == player)),
                iGVA = sum(event_type == "GIVEAWAY" & p1 == player),
                iTKA = sum(event_type == "TAKEAWAY" & p1 == player),
                iHF = sum(event_type == "HIT" & p1 == player),
                iHA = sum(event_type == "HIT" & p2 == player),
                iBLK = sum(event_type == "BLOCKED_SHOT" & p2 == player),
                iFOW = sum(event_type == "FACEOFF" & p1 == player),
                iFOL = sum(event_type == "FACEOFF" & p2 == player),
                iPENT2 = sum(na.omit(1*(event_type == "PENL" & p1 == player) +
                                     1*(event_type == "PENL" & p1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENL" & p1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPENT5 = sum(na.omit(event_type == "PENL" & p1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                iPEND2 = sum(na.omit(1*(event_type == "PENL" & p2 == player) +
                                     1*(event_type == "PENL" & p2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                     1*(event_type == "PENL" & p2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                     )
                             ),
                iPEND5 = sum(na.omit(event_type == "PENL" & p2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
                ) %>%
      data.frame() %>%
      return()
    
  }
  
}

# Summarize Goalie Stats (Old PBP Format)
st.old_sum_goalie <- function(x, venue) {
  
  ## Description
  # old_sum_goalie() summarizes all goalie counting stats from a Corsica 1.0 PBP data frame object
  # x is expected to be a grouped data frame with home_goalie or away_goalie as a grouping variable for \
  # venue = "home" and venue = "away" respectively
  
  venue_ <- tolower(as.character(venue))
  
  if(venue_ == "home") {
    
    x %>%
      rename(player = home_goalie) %>%
      summarise(venue = "Home",
                GP = length(unique(game_id)),
                TOI = sum(nabs(Event.Length))/60,
                CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                SA = sum(event_type %in% st.shot_events & event_team == away_team),
                GA = sum(event_type == "GOAL" & event_team == away_team),
                xGA = sum(na.omit((prob_goal/(prob_goal + prob_save))*(event_type %in% st.shot_events & event_team == away_team)))
                ) %>%
      data.frame() %>%
      return()
    
  } else if(venue_ == "away") {
    
    x %>%
      rename(player = away_goalie) %>%
      summarise(venue = "Away",
                GP = length(unique(game_id)),
                TOI = sum(nabs(Event.Length))/60,
                CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                SA = sum(event_type %in% st.shot_events & event_team == home_team),
                GA = sum(event_type == "GOAL" & event_team == home_team),
                xGA = sum(na.omit((prob_goal/(prob_goal + prob_save))*(event_type %in% st.shot_events & event_team == home_team)))
                ) %>%
      data.frame() %>%
      return()
    
  }
  
}

