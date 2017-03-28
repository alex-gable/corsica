### DRYSCRAPE ###
# Last edit: Manny (2017-03-28)


## Description
# Dryscrape contains all functions and tools related to scraping data for Corsica 
# Dependencies: Rcurl, rjson, dplyr, lubridate, user_functions


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate)


## Objects
c(20001:21230,
  30111:30117, 30121:30127, 30131:30137, 30141:30147, 30151:30157, 30161:30167, 30171:30177, 30181:30187,
  30211:30217, 30221:30227, 30231:30237, 30241:30247,
  30311:30317, 30321:30327,
  30411:30417
  ) %>% 
  as.character() ->
  ds.all_games

c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
  "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36"
  ) ->
  ds.user_agents


## Meta Functions
# Get PBP
ds.get_pbp <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_pbp() imports the PBP page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
               as.character(year),
               "0",
               as.character(game_id),
               "/feed/live?site=en_nhl",
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = TRUE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- fromJSON(raw_text)
  
  return(raw_json)
  
}

# Get Shifts
ds.get_shifts <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_shifts() imports the shift report page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
               as.character(year),
               "0",
               as.character(game_id),
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = TRUE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- fromJSON(raw_text)
  
  return(raw_json)
  
}

# Get Highlights
ds.get_highlights <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_highlights() imports the highlights page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
               as.character(year),
               "0",
               as.character(game_id),
               "/content?site=en_nhl",
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = TRUE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- fromJSON(raw_text)
  
  return(raw_json)
  
}

# Get Team Profile
ds.get_team_profile <- function(team_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_team_profile() imports the team profile page corresponding to a given team ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/teams/",
               as.character(team_id),
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = TRUE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- fromJSON(raw_text)
  
  return(raw_json)
  
}

# Get Player Profile
ds.get_player_profile <- function(player_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_player_profile() imports the player profile page corresponding to a given player ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/people/",
               as.character(player_id),
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = TRUE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- fromJSON(raw_text)
  
  return(raw_json)
  
}

# Parse PBP Event
ds.parse_event <- function(x) {
  
  ## Description
  # parse_event() parses a single event from the PBP JSON object and returns a data frame
  
  x$players %>%
    sapply(function(p) as.character(p$player$id)) %>%
    unlist() %>%
    c(rep(NA, 
          times = (4 - length(x$players))
          )
      ) ->
    player_ids
  
  data.frame(game_date = as.Date(x$about$dateTime),
             game_id = NA,
             event_id = nabs(x$about$eventIdx),
             event_code = as.character(x$result$eventCode),
             event_type = as.character(x$result$eventTypeId),
             event_description = as.character(x$result$description),
             event_detail = na_if_null(as.character(x$result$secondaryType)),
             datetime = parse_date_time(x$about$dateTime, "y-m-d.H:M:S."),
             game_period = nabs(x$about$period),
             period_time_elapsed = as.character(x$about$periodTime),
             period_time_remaining = as.character(x$about$periodTimeRemaining),
             event_team = na_if_null(as.character(x$team$id)),
             event_player_1 = na_if_null(player_ids[1]),
             event_player_2 = na_if_null(player_ids[2]),
             event_player_3 = na_if_null(player_ids[3]),
             event_player_4 = na_if_null(player_ids[4]),
             coords_x = na_if_null(x$coordinates$x),
             coords_y = na_if_null(x$coordinates$y)
             ) ->
    event_df
  
  return(event_df)
  
}


## General Functions
# Scrape
ds.scrape <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape() collects and parses the data for a game corresponsing to a given season and game ID
  # A list object containing _________ is returned
  
  season <- as.character(season); game_id <- as.character(game_id)
  
  year <- substr(season, 0, 4)
  
  pbp <- ds.get_pbp(year, game_id, try_tolerance, agents)
  shifts <- ds.get_shifts(year, game_id, try_tolerance, agents)
  highlights <- ds.get_highlights(year, game_id, try_tolerance, agents)
  
  do.call("rbind", 
          lapply(pbp$liveData$plays$allPlays, ds.parse_event)
          ) ->
    pbp_df

  # Continue here...
  
}

