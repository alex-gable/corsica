### COMPILE GAMES ###
# Last edit: Manny (2017-04-24)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); require(doMC); require(Kmisc); require(RSQLite)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")
load("~/Documents/github/corsica/modules/stats.RData")


## Compile games
# Scrape
game_list <- ds.compile_games(games = g,
                              season = "20162017",
                              try_tolerance = 5,
                              agents = ds.user_agents
                              )

# Unpack
pbp <- game_list[[1]]
shifts <- game_list[[2]]
highlights <- game_list[[3]]
media <- game_list[[4]]


## Write to database
# Connect
conn <- dbConnect(SQLite(), "~/Documents/corsica_data/raw.sqlite")

# Check for duplicates
pbp_contains <- sqliteQuickColumn(conn,
                                  "pbp",
                                  "game_id"
                                  )

pbp %>%
  filter(game_id %in% unique(pbp_contains) == FALSE) %>%
  data.frame() ->
  pbp

shifts_contains <- sqliteQuickColumn(conn,
                                     "shifts",
                                     "game_id"
                                     )

shifts %>%
  filter(game_id %in% unique(shifts_contains) == FALSE) %>%
  data.frame() ->
  shifts

highlights_contains <- sqliteQuickColumn(conn,
                                         "highlights",
                                         "game_id"
                                         )

highlights %>%
  filter(game_id %in% unique(highlights_contains) == FALSE) %>%
  data.frame() ->
  highlights

media_contains <- sqliteQuickColumn(conn,
                                    "media",
                                    "game_id"
                                    )

media %>%
  filter(game_id %in% unique(media_contains) == FALSE) %>%
  data.frame() ->
  media

# Write
dbWriteTable(conn,
             "pbp",
             pbp,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "shifts",
             shifts,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "highlights",
             highlights,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "media",
             media,
             overwrite = FALSE,
             append = TRUE
             )

# Disconnect
dbDisconnect(conn)

