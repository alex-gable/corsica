### FIND BROKEN FILES ###
# Last edit: Manny (2017-04-24)


## Dependencies
require(dplyr); require(doMC); require(RSQLite)
load("~/Documents/github/corsica/modules/user_functions.RData")
load("~/Documents/github/corsica/modules/dryscrape.RData")


# Set max.print
options(max.print = 10000)

## Read tables
# Connect to database
conn <- dbConnect(SQLite(), "~/Documents/corsica_data/raw.sqlite")

# PBP query
pbp_query <- dbSendQuery(conn, "SELECT season, session, game_date, game_id, event_length, home_team, away_team, home_on_1 FROM pbp")
pbp <- fetch(pbp_query, -1)

# Shifts query

# Highlights query

# Media query

dbDisconnect(conn)


## Check PBP
pbp %>%
  group_by(season, session, game_date, game_id) %>%
  summarise(TOI = sum(na.omit(event_length))/60,
            NArows = sum(is.na(home_on_1) == TRUE),
            rows = n()
            ) %>%
  data.frame() ->
  newpbp

broken <- newpbp$game_id[which(newpbp$TOI <= 55 | newpbp$NArows >= 0.5*newpbp$rows)]

c(paste("20120",
        ds.all_games,
        sep = ""
        ),
  paste("20130",
        ds.all_games,
        sep = ""
        ),
  paste("20140",
        ds.all_games,
        sep = ""
        ),
  paste("20150",
        ds.all_games,
        sep = ""
        ),
  paste("20160",
        ds.all_games,
        sep = ""
        )
 ) ->
  all_games

missing <- all_games[which(all_games %in% unique(newpbp$game_id) == FALSE)]

print(broken)
print(missing)
