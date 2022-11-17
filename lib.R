getCon <- function() {
  con <- dbConnect(RSQLite::SQLite(), "FIFA2022_sqlite.db")
  return(con)
}

# load database
LoadDb <- function() {
  # for the time being create an in-memory db
  # and load with empty df
  con <- getCon()
  df <- data.frame(id=integer(0), 
                   team1=character(''), 
                   team2=character(''), 
                   score1=integer(0), 
                   score2=integer(0),
                   cards1=integer(0),
                   cards2=integer(0))
  dbWriteTable(con, "match", df, overwrite=TRUE)
  dbDisconnect(con)
  
}


teams = c("Argentina" = "ARG",
          "Australia" = "AUS",
          "Belgium" = "BEL",
          "Brazil" = "BRA",
          "Cameroon" = "CMR",
          "Canada" = "CAN",
          "Costa Rica" = "CRC",
          "Croatia" = "CRO",
          "Denmark" = "DEN",
          "Ecuador" = "ECU",
          "England" = "ENG",
          "France" = "FRA",
          "Germany" = "GER",
          "Ghana" = "GHA",
          "Iran" = "IRN",
          "Japan" = "JPN",
          "South Korea" = "KOR",
          "Mexico" = "MEX",
          "Morocco" = "MAR",
          "Netherlands" = "NED",
          "Poland" = "POL",
          "Portugal" = "POR",
          "Qatar" = "QAT",
          "Saudi Arabia" = "KSA",
          "Senegal" = "SEN",
          "Serbia" = "SRB",
          "Spain" = "ESP",
          "Switzerland" = "SUI",
          "Tunisia" = "TUN",
          "Uruguay" = "URU",
          "USA" = "USA",
          "Wales" = "WAL")


# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              team1 = "Home Team", 
              team2 = "Away Team", 
              score1 = "Home Score",
              score2 = "Away Score",
              cards1 = "Home Cards",
              cards2 = "Away Cards")
  
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
GetNextId <- function() {
  con <- getCon()
  if (length(dbListTables(con)) == 0) {
    dbDisconnect(con)
    return(1)
  } else {
    maxid = unname(dbGetQuery(con, "SELECT max(id) from match"))
    dbDisconnect(con)
    if (is.na(maxid)) {
      return(1)
    } else {
      return(unlist(maxid) + 1)
    }
  }
}

#C - create
CreateData <- function(data) {
  data <- CastData(data)
  data["id"] <- GetNextId()
  
  con <- getCon()
  dbWriteTable(con, "match", data, append=TRUE)
  dbDisconnect(con)
}

#R - read
ReadData <- function() {
  con <- getCon()
  if (length(dbListTables(con)) == 0) {
    dbDisconnect(con)
    LoadDb()
  } else {
    res = dbReadTable(con, 'match')
    dbDisconnect(con)
    if (nrow(res)) {
      rownames(res) <- unlist(res['id'])
      return(res[-1])
    }
  }
}


#U - update
UpdateData <- function(data) {
  data <- CastData(data)
  con <- getCon()
  dbBegin(con)
  # first delete the entry to update
  rows = dbExecute(con,sprintf("DELETE FROM match where id = %s",unname(data["id"])))
  if (rows == 1) {
    # then, write a new entry with the updated data
    dbWriteTable(con, "match", data, append=TRUE)
    dbCommit(con)
  } else {
    # rollback if there's a problem
    dbRollback(con)
  }
  dbDisconnect(con)
}

#D - delete
DeleteData <- function(data) {
  con <- getCon()
  dbExecute(con, sprintf("DELETE FROM match WHERE id = %s", unname(data["id"])))
  dbDisconnect(con)
}



# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(id = as.integer(data["id"]),
                      team1 = data["team1"], 
                      team2 = data["team2"],
                      score1 = as.integer(data["score1"]),
                      score2 = as.integer(data["score2"]),
                      cards1 = as.integer(data["cards1"]),
                      cards2 = as.integer(data["cards2"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", 
                             team1 = "ENG", 
                             team2 = "SCO", 
                             score1 = "0", 
                             score2 = "0",
                             cards1 = "0",
                             cards2 = "0"))
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "team1", value = unname(data["team1"]))
  updateTextInput(session, "team2", value = unname(data["team2"]))
  updateSliderInput(session, "score1", value = as.integer(data["score1"]))
  updateSliderInput(session, "score2", value = as.integer(data["score2"]))
  updateSliderInput(session, "cards1", value = as.integer(data["cards1"]))
  updateSliderInput(session, "cards2", value = as.integer(data["cards2"]))
}

# scoring functions
topScoringTeam <- function() {
  con <- getCon()
  res = dbGetQuery(con, "SELECT team1 as Team, sum(score1) as MostGoals FROM (
                   SELECT m1.team1, m1.team2, m1.score1, m1.score2 
                   FROM match m1 
                   UNION select m2.team2, m2.team1, m2.score2, m2.score1 
                   FROM match m2
  ) AS foo 
                   GROUP BY team1 
                   ")
  dbDisconnect(con)
  return(res)
}

# scoring functions
topConcedingTeam <- function() {
  con <- getCon()
  res = dbGetQuery(con, "SELECT team1 as Team, sum(score2) as MostGoals FROM (
                   SELECT m1.team1, m1.team2, m1.score1, m1.score2 
                   FROM match m1 
                   UNION select m2.team2, m2.team1, m2.score2, m2.score1 
                   FROM match m2
  ) AS foo 
                   GROUP BY team1 
                   ")
  dbDisconnect(con)
  return(res)
}

# find teams with most cards
mostCards <- function() {
  con <- getCon()
  res = dbGetQuery(con, "SELECT team1 as Team, sum(cards1) as MostCards FROM (
                   SELECT m1.team1, m1.team2, m1.cards1, m1.cards2 
                   FROM match m1 
                   UNION select m2.team2, m2.team1, m2.cards2, m2.cards1 
                   FROM match m2
  ) AS foo 
                   GROUP BY team1")
  dbDisconnect(con)
  return(res)
}

# Get the games played per team
gamesPlayed <- function() {
  con <- getCon()
  res = dbGetQuery(con, "select team1 as team, count(*) as games from (SELECT m1.team1, m1.team2 from match m1 union select m2.team2, m2.team1 from match m2) as foo group by team1")
  dbDisconnect(con)
  return(res)
}

# Calculate the number of cards per game
cardsPerGame <- function() {
  gp = gamesPlayed()
  mc = mostCards()
  # merge games played with the cards collected
  df = data.frame(mc, Games = gp$games, cpg = mc$MostCards/gp$games)
  # order by cards per game
  df = df[order(df$cpg, decreasing = TRUE),]
  # keep top five
  df =  df[1:6,]
  df$Team <- factor(df$Team, levels=df$Team)
  return(df)
}
