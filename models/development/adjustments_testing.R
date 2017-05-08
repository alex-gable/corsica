### ADJUSTMENTS TESTING ###
# Last edit: Manny (2017-05-06)

## Dependencies
require(dplyr); require(RSQLite); require(doMC); require(Kmisc); require(survival); require(glmnet)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/stats.RData")
load("~/Documents/github/corsica/models/xg_model.RData")

# load("~/Documents/github/corsica/models/development/adjustments_testing_data.RData")

## Load data
# Connect to database
conn <- dbConnect(SQLite(), "~/Documents/corsica_data/raw.sqlite")

# Query PBP
pbp_query <- dbSendQuery(conn, "SELECT * FROM pbp WHERE event_type IN ('FACEOFF', 'GOAL', 'SHOT', 'MISSED_SHOT', 'BLOCKED_SHOT')")

pbp <- fetch(pbp_query, -1)

# Disconnect
dbDisconnect(conn)


## Prepare data
# Add event rinkside 
pbp$event_rinkside <- st.which_zone(pbp$coords_x)

# Add faceoff reference
pbp %>%
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
  newpbp


## State summary
newpbp %>%
  filter(game_strength_state %in% c("5v5",
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
  group_by(game_strength_state, home_score_adv, home_zonestart) %>%
  summarise(CF = sum(is_home_team == 1 & event_type %in% st.corsi_events),
            CA = sum(is_home_team == 0 & event_type %in% st.corsi_events)
            ) %>%
  data.frame() ->
  state_sum


## Cox Regression
# Prepare model data
newpbp %>%
  filter(game_strength_state %in% c("5v5",
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
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(elapsed = game_seconds - lag(game_seconds, 1),
         hazard_goal = 1*(event_type == "GOAL"),
         hazard_shot = 1*(event_type %in% st.shot_events),
         hazard_fenwick = 1*(event_type %in% st.fenwick_events),
         hazard_corsi = 1*(event_type %in% st.corsi_events)
         ) %>%
  ungroup() %>%
  select(event_type,
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

# Save
save(list = "model_data", file = "~/Documents/github/corsica/models/development/adjustments_testing_data.RData")

# Model home Corsi rate
model_mat <- model.matrix(hazard_corsi ~
                          as.factor(home_zonestart)*seconds_since + game_strength_state + home_score_adv*game_seconds,
                          data = model_data
                          )

surv <- Surv(model_data$elapsed, model_data$hazard_corsi*model_data$is_home_team)

cox_home_corsi <- cv.glmnet(x = model_mat,
                            y = surv,
                            family = "cox",
                            nfolds = 10,
                            nlambda = 100,
                            alpha = 0
                            )

cox_home_corsi$glmnet.fit$dev.ratio[which(round(cox_home_corsi$lambda, 4) == round(cox_home_corsi$lambda.min, 4))]
# %Dev = 0.01777632

# Model home Fenwick rate
model_mat <- model.matrix(hazard_fenwick ~
                          as.factor(home_zonestart)*seconds_since + game_strength_state + home_score_adv*game_seconds,
                          data = model_data
                          )

surv <- Surv(model_data$elapsed, model_data$hazard_fenwick*model_data$is_home_team)

cox_home_fenwick <- cv.glmnet(x = model_mat,
                              y = surv,
                              family = "cox",
                              nfolds = 10,
                              nlambda = 100,
                              alpha = 0
                              )

cox_home_fenwick$glmnet.fit$dev.ratio[which(round(cox_home_fenwick$lambda, 4) == round(cox_home_fenwick$lambda.min, 4))]
# %Dev = 0.01505101
