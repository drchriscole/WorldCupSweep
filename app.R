##
## Shiny app for updating and monitoring a World Cup sweepstake
##

library(shiny)
library(shinyjs)
library(DBI)
library(ggplot2)

# much of this code is from this blog:
# https://ipub.com/shiny-crud-app/

# load database
LoadDb <- function() {
  # for the time being create an in-memory db
  # and load with empty df
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
          "Colombia" = "COL",
          "Costa Rica" = "CRC",
          "Croatia" = "CRO",
          "Denmark" = "DEN",
          "Egypt" = "EGY",
          "England" = "ENG",
          "France" = "FRA",
          "Germany" = "GER",
          "Iceland" = "ISL",
          "Iran" = "IRN",
          "Japan" = "JPN",
          "Mexico" = "MEX",
          "Morocco" = "MAR",
          "Nigeria" = "NGA",
          "Panama" = "PAN",
          "Peru" = "PER",
          "Poland" = "POL",
          "Portugal" = "POR",
          "Russia" = "RUS",
          "Saudi Arabia" = "KSA",
          "Senegal" = "SEN",
          "Serbia" = "SRB",
          "South Korea" = "KOR",
          "Spain" = "ESP",
          "Sweden" = "SWE",
          "Switzerland" = "SUI",
          "Tunisia" = "TUN",
          "Uruguay" = "URU")

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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
  print(data)
  data <- CastData(data)
  #print(data)
  data["id"] <- GetNextId()

  print("here2")
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
  dbWriteTable(con, "match", data, append=TRUE)
  dbDisconnect(con)
}

#R - read
ReadData <- function() {
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
                             team1 = "ARG", 
                             team2 = "AUS", 
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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
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

ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  fluidRow(
    column(3,
      #input fields
      shinyjs::disabled(textInput("id", "Id", "0")),
      selectInput("team1", "Home Team", teams),
      selectInput("team2", "Away Team", teams, selected = "AUS"),
      sliderInput("score1", "Home Score", 0, 10, 0, ticks = TRUE),
      sliderInput("score2", "Away Score", 0, 10, 0, ticks = TRUE),
      sliderInput("cards1", "Home Cards", 0, 10, 0, ticks = TRUE),
      sliderInput("cards2", "Away Cards", 0, 10, 0, ticks = TRUE),
      
      #action buttons
      actionButton("submit", "Submit"),
      actionButton("new", "New"),
      actionButton("delete", "Delete")
    ),
    
    column(8,
      tabsetPanel(type = "tabs",
        tabPanel("Plots", plotOutput('scoresPlot'), plotOutput('cardsPlot') ),
        tabPanel("Table", DT::dataTableOutput("responses"))
        
      )
    )
  )
)


server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display plot
  output$scoresPlot <- renderPlot({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ts <- topScoringTeam()
    ts$Team <- factor(ts$Team, levels=ts$Team)
    tc <- topConcedingTeam()
    tc$Team <- factor(tc$Team, levels=tc$Team)
    maxScore = max(tc$MostGoals, ts$MostGoals)
    tc$MostGoals <- tc$MostGoals * -1
    par(mfrow=c(2,1))
    par(mai=c(1,0.8,0,0))
    par(mgp=c(1.5,0.5,0.5))
    par(mar=c(0,3,3,0))
    # need to plot twice in order to have lines below bars
    barplot(rep(NA, length(ts$MostGoals)), ylim=c(0,maxScore), axes = FALSE)
    abline(h=seq(0,maxScore,2), col='lightgray')
    barplot(ts$MostGoals, col="darkgreen", ylab='Scored', las=1, tck=-0.02, cex.names = 0.5, cex.axis = 0.8, ylim=c(0,maxScore), add = TRUE)
    par(mai=c(1,0.8,0,0))
    par(mar=c(5,3,0,0))
    barplot(rep(NA, length(tc$MostGoals)), ylim=c(-maxScore,0), axes = FALSE)
    abline(h=seq(-maxScore,0,2), col='lightgray')
    barplot(tc$MostGoals, 
            names.arg = tc$Team, 
            col="red", 
            ylab='Conceeded', 
            las=1, 
            tck=-0.02, 
            cex.names = 0.65, 
            cex.axis = 0.8,
            add = TRUE)


  })
  
  # display plot
  output$cardsPlot <- renderPlot({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    df = cardsPerGame()
    ggplot(df, aes(x=Team, y = cpg)) + 
      geom_col(colour='black', fill='yellow') +
      xlab("") +
      ylab("CardsPerGame") +
      ggtitle("Most Cards/Game") +
      theme_bw() +
      theme(axis.text = element_text(size=14), axis.title = element_text(size=15), title = element_text(size=15, face='bold'))
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, 
  selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1],
  options = list(
    pageLength = 25
  )
  )     
}


# Shiny app with 3 fields that the user can submit data for
shinyApp(ui = ui, server = server)