### DRYSCRAPE ###
# Last edit: Manny (2017-03-28)


## Description
# Dryscrape contains all functions and tools related to scraping data for Corsica 
# Dependencies: Rcurl, rjson, dplyr, lubridate, doMC, user_functions


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC)


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

# Get Media
ds.get_media <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_media() imports the media page corresponding to a given year and game ID and returns a JSON list object
  
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

# Get Highlights
ds.get_highlights <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_highlights() imports the highlights page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("http://live.nhle.com/GameData/",
               as.character(season),
               "/",
               substr(as.character(season), 0, 4),
               "0",
               as.character(game_id),
               "/gc/gcgm.jsonp",
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
  
  clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)
  
  raw_json <- fromJSON(clean_text)
  
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
             coords_y = na_if_null(x$coordinates$y),
             highlight_id = nabs(x$about$eventId)
             ) ->
    event_df
  
  return(event_df)
  
}

# Parse Shift
ds.parse_shift <- function(x) {
  
  ## Description
  # parse_shift() parses a single shift from the Shifts JSON object and returns a data frame
  
  data.frame(game_date = NA,
             game_id = nabs(x$gameId),
             shift_number = nabs(x$eventNumber),
             shift_start = as.character(x$startTime),
             shift_end = as.character(x$endTime),
             shift_duration = na_if_null(as.character(x$duration)),
             team_id = as.character(x$teamId),
             player_id = as.character(x$playerId)
             ) ->
    shift_df
  
  return(shift_df)
  
}

# Parse Highlight
ds.parse_highlight <- function(x) {
  
  ## Description
  # parse_highlight() parses a single highlight from the Highlights JSON object and returns a data frame
  
  data.frame(game_date = NA,
             game_id = NA,
             event_id = x$id,
             highlight_id = x$feeds[[1]]$neulionId,
             event_team_1 = x$t1,
             event_team_2 = x$t2,
             event_period = x$p,
             event_seconds = x$sip,
             event_type = x$type
             ) ->
    highlight_df
  
  return(highlight_df)
  
}

# Parse Media
ds.parse_media <- function(x) {
  
  ## Description
  # parse_media() parses a single highlight from the Media JSON object and returns a data frame
  
  data.frame(highlight_id = as.character(x$id),
             highlight_title = as.character(x$title),
             highlight_blurb = as.character(x$blurb),
             highlight_description = as.character(x$description),
             highlight_image_url = as.character(x$image$cuts$`1136x640`$src)
             ) ->
    media_df
  
  return(media_df)
  
}


## General Functions
# Scrape
ds.scrape <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape() collects and parses the data for a game corresponsing to a given season and game ID
  # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights, [[4]] = Media) is returned
  
  season <- as.character(season); game_id <- as.character(game_id)
  
  year <- substr(season, 0, 4)
  
  pbp <- ds.get_pbp(year, game_id, try_tolerance, agents)
  shifts <- ds.get_shifts(year, game_id, try_tolerance, agents)
  media <- ds.get_media(year, game_id, try_tolerance, agents)
  highlights <- ds.get_highlights(season, game_id, try_tolerance, agents)
  
  pbp_df <- dcapply(pbp$liveData$plays$allPlays, 
                    ds.parse_event, 
                    "rbind", 
                    cores = 1
                    )

  shift_df <- dcapply(shifts$data,
                      ds.parse_shift,
                      "rbind",
                      cores = 1
                      )
  
  highlight_df <- dcapply(highlights$video$events,
                          ds.parse_highlight,
                          "rbind",
                          cores = 1
                          )
  
  media_df <- dcapply(media$highlights$gameCenter$items,
                      ds.parse_media,
                      "rbind",
                      cores = 1
                      )
  
  full_highlight <- merge(highlight_df,
                          media_df,
                          by.x = "highlight_id",
                          by.y =  "highlight_id"
                          )
  
  media_preview_headline <- media$editorial$preview$items[[1]]$headline
  media_preview_subhead <- media$editorial$preview$items[[1]]$subhead
  media_preview_description <- media$editorial$preview$items[[1]]$seoDescription
  
  if(length(media$editorial$recap$items) > 0) {
    
    media_recap_headline <- na_if_null(media$editorial$recap$items[[1]]$headline)
    media_recap_subhead <- na_if_null(media$editorial$recap$items[[1]]$subhead)
    media_recap_description <- na_if_null(media$editorial$recap$items[[1]]$seoDescription)
    
  } else {
    
    media_recap_headline <- NA
    media_recap_subhead <- NA
    media_recap_description <- NA
    
  }
  
  full_media <- c("preview_headline" = na_if_null(media_preview_headline),
                  "preview_subhead" = na_if_null(media_preview_subhead),
                  "preview_description" = na_if_null(media_preview_description),
                  "recap_headline" = na_if_null(media_recap_headline),
                  "recap_subhead" = na_if_null(media_recap_subhead),
                  "recap_description" = na_if_null(media_recap_description)
                  )
  
  game_list <- list(pbp_df,
                    shift_df,
                    full_highlight,
                    full_media
                    )
  
  return(game_list)
  
}

