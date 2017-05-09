### SCRAPE LIVE ###
# Last edit: Manny (2017-05-09)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC)
require(Kmisc); require(RSQLite); require(glmnet); require(rvest); require(survival)
require(caret)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")
load("~/Documents/github/corsica/modules/stats.RData")
load("~/Documents/github/corsica/models/xg_model.RData")
load("~/Documents/github/corsica/models/adjustments_model.RData")


## Functions
do_plays <- function(x) {
  
  event <- x$result$event
  type <- x$result$eventTypeId
  period <- x$about$period
  time <- x$about$periodTime
  xc <- x$coordinates$x
  yc <- x$coordinates$y
  
  if(is.null(xc) == TRUE) {xc <- NA}
  if(is.null(yc) == TRUE) {yc <- NA}
  
  c(event, type, period, time, xc, yc)
  
}


## Switches
current_season <- "20162017"


## Scrape Games
# Get schedule
sched <- ds.scrape_schedule(Sys.Date(), Sys.Date(), try_tolerance = 3, agents = ds.user_agents)

# Get game list
games <- substr(sched$game_id, 6, 10)

# Compile games
pbp <- ds.old_scrape(games, current_season, names = TRUE, pause = 1)

# Rename columns
pbp %>%
  mutate(X5 = nabs(X5),
         X6 = nabs(X6),
         event_index = cumsum(!is.na(Game.ID)),
         game_seconds = nabs(Seconds),
         home_score = nabs(Home.Score),
         away_score = nabs(Away.Score)
         ) %>%
  rename(coords_x = X5,
         coords_y = X6,
         event_type = Event,
         game_period = Period,
         event_description = Description,
         game_date = Date,
         season = Season,
         session = Season.Type,
         game_id = Game.ID,
         event_team = ev.team,
         game_strength_state = Strength.State,
         game_score_state = Score.State,
         home_goalie = Home.Goalie,
         away_goalie = Away.Goalie,
         home_skaters = Home.Skaters,
         away_skaters = Away.Skaters,
         home_team = Home.Team,
         away_team = Away.Team,
         event_detail = Detail
         ) %>%
  data.frame() ->
  pbp

# Rename events
pbp$event_type[which(pbp$event_type == "FAC")] <- "FACEOFF"
pbp$event_type[which(pbp$event_type == "TAKE")] <- "TAKEAWAY"
pbp$event_type[which(pbp$event_type == "BLOCK")] <- "BLOCKED_SHOT"
pbp$event_type[which(pbp$event_type == "MISS")] <- "MISSED_SHOT"
pbp$event_type[which(pbp$event_type == "GIVE")] <- "GIVEAWAY"

# Rename session
pbp$session[which(pbp$session == "Regular")] <- "R"
pbp$session[which(pbp$session == "Playoffs")] <- "P"

# Add rinkside
pbp %>%
  group_by(game_period) %>%
  summarise(home_right = sum(event_type %in% st.fenwick_events & coords_x > 25 & event_team == home_team),
            home_left = sum(event_type %in% st.fenwick_events & coords_x < -25 & event_team == home_team)
            ) %>%
  mutate(home_side = as.factor(1*(home_right > home_left) + 2*(home_left > home_right)),
         away_side = as.factor(3 - nabs(home_side))
         ) %>%
  select(game_period, home_side, away_side) %>%
  data.frame() ->
  rinkside_df
    
levels(rinkside_df$home_side) <- c("L", "R")
levels(rinkside_df$away_side) <- c("L", "R")

pbp$home_rinkside = rinkside_df$home_side[match(pbp$game_period, rinkside_df$game_period)]
pbp$away_rinkside = rinkside_df$away_side[match(pbp$game_period, rinkside_df$game_period)]

# Enhance PBP
pbp <- st.pbp_enhance(pbp)


## Compile Team Stats
bind_rows(
  pbp %>%
    group_by(season, session, game_id, game_date, home_team, game_strength_state) %>%
    st.old_sum_team("Home"),
  
  pbp %>%
    group_by(season, session, game_id, game_date, away_team, game_strength_state) %>%
    st.old_sum_team("Away")
) %>%
  data.frame() ->
  team_stats_full

## Compile Skater Stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, h1.num, game_strength_state) %>%
    rename(player = h1.num) %>%
    st.old_sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, h2.num, game_strength_state) %>%
    rename(player = h2.num) %>%
    st.old_sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, h3.num, game_strength_state) %>%
    rename(player = h3.num) %>%
    st.old_sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, h4.num, game_strength_state) %>%
    rename(player = h4.num) %>%
    st.old_sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, h5.num, game_strength_state) %>%
    rename(player = h5.num) %>%
    st.old_sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, h6.num, game_strength_state) %>%
    rename(player = h6.num) %>%
    st.old_sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, a1.num, game_strength_state) %>%
    rename(player = a1.num) %>%
    st.old_sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, a2.num, game_strength_state) %>%
    rename(player = a2.num) %>%
    st.old_sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, a3.num, game_strength_state) %>%
    rename(player = a3.num) %>%
    st.old_sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, a4.num, game_strength_state) %>%
    rename(player = a4.num) %>%
    st.old_sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, a5.num, game_strength_state) %>%
    rename(player = a5.num) %>%
    st.old_sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, a6.num, game_strength_state) %>%
    rename(player = a6.num) %>%
    st.old_sum_skater("Away")
) %>%
  data.frame() %>%
  group_by(season, session, game_id, game_date, player, game_strength_state, venue) %>%
  summarise_each(funs(sum), -c(GP)) %>%
  data.frame() %>%
  merge(team_stats_full %>%
          rename(tTOI = TOI, 
                 tCF = CF, 
                 tCA = CA, 
                 tFF = FF, 
                 tFA = FA, 
                 tSF = SF, 
                 tSA = SA, 
                 tGF = GF, 
                 tGA = GA, 
                 txGF = xGF, 
                 txGA = xGA, 
                 tACF = ACF, 
                 tACA = ACA, 
                 tAFF = AFF, 
                 tAFA = AFA, 
                 tASF = ASF, 
                 tASA = ASA, 
                 tAGF = AGF, 
                 tAGA = AGA, 
                 tAxGF = AxGF, 
                 tAxGA = AxGA,
                 tOZS = OZS, 
                 tDZS = DZS, 
                 tNZS = NZS
                 ) %>%
          select(team, 
                 season, 
                 game_id, 
                 venue, 
                 game_strength_state,
                 tTOI:tNZS
                 ) %>%
          data.frame(),
        by.x = c("season", "game_id", "venue", "game_strength_state"),
        by.y = c("season", "game_id", "venue", "game_strength_state")
        ) %>%
  data.frame() ->
  skater_stats_full

## Compile Goalie Stats
bind_rows(
  pbp %>%
    group_by(season, session, game_id, game_date, home_goalie, game_strength_state) %>%
    st.old_sum_goalie("Home"),
  
  pbp %>%
    group_by(season, session, game_id, game_date, away_goalie, game_strength_state) %>%
    st.old_sum_goalie("Away")
) %>%
  data.frame() ->
  goalie_stats_full


## Save file
foreach(i = unique(pbp$game_id)) %do% {

  pbp <- filter(pbp, game_id == i) %>% data.frame()
  team_stats_full <- filter(team_stats_full, game_id == i) %>% data.frame()
  skater_stats_full <- filter(skater_stats_full, game_id == i) %>% data.frame()
  goalie_stats_full <- filter(goalie_stats_full, game_id == i) %>% data.frame()
  
  save(list = c("pbp", "team_stats_full", "skater_stats_full", "goalie_stats_full"),
       file = paste("~/Documents/corsica_data/game_",
                    current_season,
                    "0",
                    i,
                    ".RData",
                    sep = ""
                    )
       )
  
  TRUE

}

