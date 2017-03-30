### DRYSCRAPE ###
# Last edit: Manny (2017-03-29)


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
                                     verbose = FALSE,
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
                                     verbose = FALSE,
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
                                     verbose = FALSE,
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
                                     verbose = FALSE,
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
                                     verbose = FALSE,
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
                                     verbose = FALSE,
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

# Get Schedule
ds.get_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_schedule() imports the schedule page corresponding to a given date range and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
               as.character(start),
               "&endDate=",
               as.character(end),
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
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
  
  data.frame(game_date = NA,
             game_id = NA,
             season = NA,
             session = NA,
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
             season = NA,
             session = NA,
             shift_number = na_if_null(nabs(x$eventNumber)),
             shift_period = nabs(x$period),
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
             season = NA,
             session = NA,
             event_id = x$id,
             highlight_id = x$feeds[[1]]$neulionId,
             event_team_1 = na_if_null(x$t1),
             event_team_2 = na_if_null(x$t2),
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

# Parse Game
ds.parse_game <- function(x) {
  
  ## Description
  # parse_game() parses a single game from the Schedule >> Date JSON object and returns a data frame
  # parse_game() is an inner function for parse_date()
  
  data.frame(game_id = nabs(x$gamePk),
             game_date = as.Date(x$gameDate),
             season = as.character(x$season),
             session = as.character(x$gameType),
             game_status = as.character(x$status$detailedState),
             away_team_id = nabs(x$teams$away$team$id),
             home_team_id = nabs(x$teams$home$team$id),
             game_venue = na_if_null(as.character(x$venue$name)),
             game_datetime = parse_date_time(x$gameDate, "y-m-d.H:M:S.")
             ) ->
    game_df
  
  return(game_df)
  
}

# Parse Date
ds.parse_date <- function(x) {
  
  ## Description
  # parse_date() parses a single date from the Schedule JSON object and returns a data frame
  # parse_date() uses an inner function parse_game()
  
  date_df <- dcapply(x$games,
                     ds.parse_game,
                     "rbind",
                     cores = 1
                     )
  
  return(date_df)
  
}

# Seconds from MS
ds.seconds_from_ms <- function(ms) {
  
  ## Description
  # seconds_from_ms() returns a numeric vector of representation in seconds of a given vector in M:S format
  
  strsplit(as.character(ms), ":") %>%
    unlist() %>%
    nabs() %>%
    matrix(ncol = 2,
           byrow = TRUE
           ) ->
    time_mat
  
  seconds <- 60*time_mat[, 1] + time_mat[, 2]

  return(seconds)
    
}

# Is On
ds.is_on <- function(player_id, pbp, venue) {
  
  ## Description
  # is_on() returns a numeric vector indicating 1 if a given player is on ice during the event corresponding to the \
  # row index in the given PBP pbject
  
  if(venue == "Home") {
    
    data.frame(cumsum(1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$home_team) - 
                      1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$home_team)
                      )
              ) ->
      is_on
    
  } else if(venue == "Away") {
    
    data.frame(cumsum(1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$away_team) - 
                      1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$away_team)
                      )
              ) ->
      is_on
    
  }
  
  colnames(is_on) <- player_id
  
  return(is_on)
  
}


## General Functions
# Who
ds.who <- function(player_id) {
  
  ## Description
  # who() searches a given player ID and returns the player's full name
  
  player <- ds.get_player_profile(player_id)
  
  full_name <- player$people[[1]]$fullName
  
  return(full_name)
  
}

# Scrape
ds.scrape_game <- function(game_id, season, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_game() collects and parses the data for a game corresponsing to a given season and game ID
  # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights, [[4]] = Media) is returned
  
  season_ <- as.character(season); game_id_ <- as.character(game_id)
  
  year <- substr(season_, 0, 4)
  
  pbp <- ds.get_pbp(year, game_id_, try_tolerance, agents)
  shifts <- ds.get_shifts(year, game_id_, try_tolerance, agents)
  media <- ds.get_media(year, game_id_, try_tolerance, agents)
  highlights <- ds.get_highlights(season_, game_id_, try_tolerance, agents)
  
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
  
  game_date_ <- as.Date(pbp$gameData$datetime$dateTime)
  session_ <- as.character(pbp$gameData$game$type)
  game_id_unique <- nabs(pbp$gameData$game$pk)
  game_venue_ <- as.character(pbp$gameData$venue$name)
  home_team_ <- nabs(pbp$gameData$teams$home$id)
  away_team_ <- nabs(pbp$gameData$teams$away$id)
  
  pbp_df %>%
    mutate(game_date = game_date_,
           game_id = game_id_unique,
           season = as.character(season_),
           session = session_,
           home_team = home_team_,
           away_team = away_team_,
           game_venue = game_venue_
           ) %>%
    data.frame() ->
    pbp_df
  
  shift_df %>%
    mutate(game_date = game_date_,
           season = as.character(season_),
           session = session_,
           home_team = home_team_,
           away_team = away_team_,
           game_venue = game_venue_
           ) %>%
    data.frame() ->
    shift_df
  
  highlight_df %>%
    mutate(game_date = game_date_,
           game_id = game_id_unique,
           season = as.character(season_),
           session = session_,
           home_team = home_team_,
           away_team = away_team_,
           game_venue = game_venue_
           ) %>%
    data.frame() ->
    highlight_df
  
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
  
  full_media <- data.frame(game_date = game_date_,
                           game_id = game_id_unique,
                           season = as.character(season_),
                           session = session_,
                           home_team = home_team_,
                           away_team = away_team_,
                           preview_headline = na_if_null(media_preview_headline),
                           preview_subhead = na_if_null(media_preview_subhead),
                           preview_description = na_if_null(media_preview_description),
                           recap_headline = na_if_null(media_recap_headline),
                           recap_subhead = na_if_null(media_recap_subhead),
                           recap_description = na_if_null(media_recap_description)
                           )
  
  game_list <- list(pbp_df,
                    shift_df,
                    full_highlight,
                    full_media
                    )
  
  return(game_list)
  
}

# Scrape Team Profile
ds.scrape_team_profile <- function(team_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_team_profile() collects and parses the data for a team corresponsing to a given team ID
  # A data frame is returned
  
  team_id_ <- nabs(team_id)
  
  team <- ds.get_team_profile(team_id_, try_tolerance, agents)
  
  data.frame(team_id = nabs(team$teams[[1]]$id),
             team_name = team$teams[[1]]$name,
             team_alias = team$teams[[1]]$abbreviation,
             team_venue = na_if_null(team$teams[[1]]$venue$name),
             team_location = na_if_null(team$teams[[1]]$locationName),
             team_city = na_if_null(team$teams[[1]]$venue$city),
             team_division_id = na_if_null(nabs(team$teams[[1]]$division$id)),
             team_division_name = na_if_null(team$teams[[1]]$division$name),
             team_conference_id = na_if_null(nabs(team$teams[[1]]$conference$id)),
             team_conference_name = na_if_null(team$teams[[1]]$conference$name),
             franchise_id = nabs(team$teams[[1]]$franchiseId),
             is_active = as.logical(team$teams[[1]]$active)
             ) ->
    team_df
  
  return(team_df)
  
}

# Scrape Player Profile
ds.scrape_player_profile <- function(player_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_player_profile() collects and parses the data for a player corresponsing to a given player ID
  # A data frame is returned
  
  player_id_ <- nabs(player_id)
  
  player <- ds.get_player_profile(player_id_, try_tolerance, agents)
  
  data.frame(player_id = nabs(player$people[[1]]$id),
             player_name_first = as.character(player$people[[1]]$firstName),
             player_name_last = as.character(player$people[[1]]$lastName),
             player_name_full = as.character(player$people[[1]]$fullName),
             player_jerseynum = na_if_null(nabs(player$people[[1]]$primaryNumber)),
             player_position = na_if_null(as.character(player$people[[1]]$primaryPosition$code)),
             player_birth_date = na_if_null(as.Date(player$people[[1]]$birthDate)),
             player_birth_city = na_if_null(as.character(player$people[[1]]$birthCity)),
             player_birth_country = na_if_null(as.character(player$people[[1]]$birthCountry)),
             player_nationality = na_if_null(as.character(player$people[[1]]$nationality)),
             player_height = na_if_null(as.character(player$people[[1]]$height)),
             player_weight = na_if_null(nabs(player$people[[1]]$weight)),
             player_handedness = na_if_null(as.character(player$people[[1]]$shootsCatches)),
             is_active = as.logical(player$people[[1]]$active),
             is_rookie = as.logical(player$people[[1]]$rookie)
             ) ->
    player_df
  
  return(player_df)
  
}

# Scrape Schedule
ds.scrape_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_schedule() collects and parses the schedule data for a range corresponsing to a given start and end date
  # A data frame is returned
  
  start_ <- as.character(start); end_ <- as.character(end)
  
  sched <- ds.get_schedule(start_, end_, try_tolerance, agents)
  
  sched_df <- dcapply(sched$dates,
                      ds.parse_date, 
                      "rbind", 
                      cores = 1
                      )
  
  return(sched_df)
  
}

# Compile Games
ds.compile_games <- function(games, season, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # compile_games() collects, parses and compiles all game data corresponding to a given vector of game IDs and season
  # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights, [[4]] = Media) is returned
  
  foreach(g = as.character(games)) %do% {
    
    cat(g,
        "...",
        "\n",
        sep = ""
        )
    
    ds.scrape_game(g, season, try_tolerance, agents)
    
  } -> nested_games
  
  unpacked <- do.call(Map, c(rbind, nested_games))
  
  pbp <- unpacked[[1]]
  shifts <- unpacked[[2]]
  highlights <- unpacked[[3]]
  media <- unpacked[[4]]
  
  pbp$game_seconds = 1200*(nabs(pbp$game_period) - 1) + ds.seconds_from_ms(pbp$period_time_elapsed)
  
  shifts$start_seconds = 1200*(nabs(shifts$shift_period) - 1) + ds.seconds_from_ms(shifts$shift_start)
  shifts$end_seconds = 1200*(nabs(shifts$shift_period) - 1) + ds.seconds_from_ms(shifts$shift_end)
  
  bind_rows(
    shifts %>%
      filter(!is.na(shift_duration)) %>%
      group_by(game_id,
               game_date,
               season,
               session,
               home_team,
               away_team,
               team_id,
               shift_number,
               start_seconds
               ) %>%
      rename(game_seconds = start_seconds, event_team = team_id) %>%
      summarise(event_type = "ON",
                game_period = nabs(first(shift_period)),
                players_substituted = paste(unique(player_id), collapse = ", ")
                ) %>%
      data.frame(),
   
     shifts %>%
       filter(!is.na(shift_duration)) %>%
       group_by(game_id,
                game_date,
                season,
                session,
                home_team,
                away_team,
                team_id,
                shift_number,
                end_seconds
                ) %>%
       rename(game_seconds = end_seconds, event_team = team_id) %>%
       summarise(event_type = "OFF",
                 game_period = nabs(first(shift_period)),
                 players_substituted = paste(unique(player_id), collapse = ", ")
                 ) %>%
       data.frame()
  ) -> shift_summary
  
  merge(pbp,
        highlights %>%
          rename(highlight_code = highlight_id) %>%
          select(game_id, event_id, highlight_code, highlight_title:highlight_image_url),
        by.x = c("game_id", "highlight_id"),
        by.y = c("game_id", "event_id"),
        all.x = TRUE
        ) %>%
    data.frame() ->
    new_pbp
  
  bind_rows(new_pbp,
            shift_summary
            ) %>%
    mutate(priority = 1*(event_type %in% c("TAKEAWAY", "GIVEAWAY", "MISSED_SHOT", "HIT", "SHOT", "BLOCKED_SHOT")) +
                      2*(event_type == "GOAL") +
                      3*(event_type == "STOP") +
                      4*(event_type == "PENALTY") +
                      5*(event_type == "OFF") +
                      6*(event_type == "ON") +
                      7*(event_type == "FACEOFF")
           ) %>% 
    group_by(game_id) %>%
    arrange(game_period,
            game_seconds,
            priority
            ) %>%
    mutate(event_index = cumsum(!is.na(game_id))) %>%
    data.frame() ->
    new_pbp
  
  home_on_mat <- dcapply(as.list(unique(shifts$player_id)),
                         ds.is_on,
                         "cbind",
                         cores = 1,
                         pbp = arrange(new_pbp,
                                       game_id,
                                       event_index
                                       ),
                         venue = "Home"
                         )
  
  away_on_mat <- dcapply(as.list(unique(shifts$player_id)),
                         ds.is_on,
                         "cbind",
                         cores = 1,
                         pbp = arrange(new_pbp,
                                       game_id,
                                       event_index
                                       ),
                         venue = "Away"
                         )
  
  which(home_on_mat == 1, 
        arr.ind = TRUE
        ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(home_on_1 = colnames(home_on_mat)[unique(col)[1]],
              home_on_2 = colnames(home_on_mat)[unique(col)[2]],
              home_on_3 = colnames(home_on_mat)[unique(col)[3]],
              home_on_4 = colnames(home_on_mat)[unique(col)[4]],
              home_on_5 = colnames(home_on_mat)[unique(col)[5]],
              home_on_6 = colnames(home_on_mat)[unique(col)[6]]
              ) %>%
    data.frame() ->
    home_on_df
  
  which(away_on_mat == 1, 
        arr.ind = TRUE
        ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(away_on_1 = colnames(away_on_mat)[unique(col)[1]],
              away_on_2 = colnames(away_on_mat)[unique(col)[2]],
              away_on_3 = colnames(away_on_mat)[unique(col)[3]],
              away_on_4 = colnames(away_on_mat)[unique(col)[4]],
              away_on_5 = colnames(away_on_mat)[unique(col)[5]],
              away_on_6 = colnames(away_on_mat)[unique(col)[6]]
              ) %>%
    data.frame() ->
    away_on_df
  
  new_pbp %>%
    arrange(game_id,
            event_index
            ) %>%
    mutate(home_on_1 = NA,
           home_on_2 = NA,
           home_on_3 = NA,
           home_on_4 = NA,
           home_on_5 = NA,
           home_on_6 = NA,
           away_on_1 = NA,
           away_on_2 = NA,
           away_on_3 = NA,
           away_on_4 = NA,
           away_on_5 = NA,
           away_on_6 = NA
           ) ->
    full_pbp
  
  full_pbp$home_on_1[home_on_df$row] <- home_on_df$home_on_1
  full_pbp$home_on_2[home_on_df$row] <- home_on_df$home_on_2
  full_pbp$home_on_3[home_on_df$row] <- home_on_df$home_on_3
  full_pbp$home_on_4[home_on_df$row] <- home_on_df$home_on_4
  full_pbp$home_on_5[home_on_df$row] <- home_on_df$home_on_5
  full_pbp$home_on_6[home_on_df$row] <- home_on_df$home_on_6
  
  full_pbp$away_on_1[away_on_df$row] <- away_on_df$away_on_1
  full_pbp$away_on_2[away_on_df$row] <- away_on_df$away_on_2
  full_pbp$away_on_3[away_on_df$row] <- away_on_df$away_on_3
  full_pbp$away_on_4[away_on_df$row] <- away_on_df$away_on_4
  full_pbp$away_on_5[away_on_df$row] <- away_on_df$away_on_5
  full_pbp$away_on_6[away_on_df$row] <- away_on_df$away_on_6
  
  new_game_list <- list(full_pbp,
                        shifts,
                        highlights,
                        media
                        )
  
  return(new_game_list)
  
}
