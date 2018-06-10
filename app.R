library(shiny)
library(shinyjs)
library(DBI)

# load database
LoadDb <- function() {
  # for the time being create an in-memory db
  # and load with empty df
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
  df <- data.frame(id=integer(0), 
                   team1=integer(0), 
                   team2=integer(0), 
                   score1=integer(0), 
                   score2=integer(0))
  dbWriteTable(con, "match", df, overwrite=TRUE)
  dbDisconnect(con)

}

teams = c("Argentina" = 1,
          "Australia" = 2,
          "Belgium" = 3,
          "Brazil" = 4,
          "Colombia" = 5,
          "Costa Rica" = 6,
          "Croatia" = 7,
          "Denmark" = 8,
          "Egypt" = 9,
          "England" = 10,
          "France" = 11,
          "Germany" = 12,
          "Iceland" = 13,
          "Iran" = 14,
          "Japan" = 15,
          "Mexico" = 16,
          "Morocco" = 17,
          "Nigeria" = 18,
          "Panama" = 19,
          "Peru" = 20,
          "Poland" = 21,
          "Portugal" = 22,
          "Russia" = 23,
          "Saudi Arabia" = 24,
          "Senegal" = 25,
          "Serbia" = 26,
          "South Korea" = 27,
          "Spain" = 28,
          "Sweden" = 29,
          "Switzerland" = 30,
          "Tunisia" = 31,
          "Uruguay" = 32)

# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              team1 = "Home Team", 
              team2 = "Away Team", 
              score1 = "Home Score",
              score2 = "Away Score")
  
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
# GetNextId <- function() {
#   if (exists("responses") && nrow(responses) > 0) {
#     max(as.integer(rownames(responses))) + 1
#   } else {
#     return (1)
#   }
# }
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

#C
# CreateData <- function(data) {
#   
#   data <- CastData(data)
#   rownames(data) <- GetNextId()
#   if (exists("responses")) {
#     responses <<- rbind(responses, data)
#   } else {
#     responses <<- data
#   }
# }
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

#R
# ReadData <- function() {
#   if (exists("responses")) {
#     responses
#   }
# }
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


#U
# UpdateData <- function(data) {
#   data <- CastData(data)
#   responses[row.names(responses) == row.names(data), ] <<- data
# }
UpdateData <- function(data) {
  data <- CastData(data)
  dbBegin(con)
  rows = dbExecute(con,sprintf("DELETE FROM match where id = %s",unname(data["id"])))
  if (rows == 1) {
    dbWriteTable(con, "match", data, append=TRUE)
    dbCommit(con)
  } else {
    dbRollback(con)
  }
}

#D
# DeleteData <- function(data) {
#   responses <<- responses[row.names(responses) != unname(data["id"]), ]
# }
DeleteData <- function(data) {
  con <- dbConnect(RSQLite::SQLite(), "sqlite.db")
  dbExecute(con, sprintf("DELETE FROM match WHERE id = %s", unname(data["id"])))
  dbDisconnect(con)
}



# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(id = as.integer(data["id"]),
                      team1 = as.integer(data["team1"]), 
                      team2 = as.integer(data["team2"]),
                      score1 = as.integer(data["score1"]),
                      score2 = as.integer(data["score2"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", team1 = "1", team2 = "2", score1 = 0, score2 = "0"))
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "team1", value = unname(data["team1"]))
  updateTextInput(session, "team2", value = unname(data["team2"]))
  updateSliderInput(session, "score1", value = as.integer(data["score1"]))
  updateSliderInput(session, "score2", value = as.integer(data["score2"]))
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
                         ORDER BY sum(score1) DESC 
                         LIMIT 1")
  dbDisconnect(con)
  return(res)
}


ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  #data table
  DT::dataTableOutput("responses", width = 300), 
  
  #input fields
  tags$hr(),
  shinyjs::disabled(textInput("id", "Id", "0")),
  selectInput("team1", "Home Team", teams),
  selectInput("team2", "Away Team", teams, selected = 2),
  sliderInput("score1", "Home Score", 0, 10, 0, ticks = TRUE),
  sliderInput("score2", "Away Score", 0, 10, 0, ticks = TRUE),

  #action buttons
  actionButton("submit", "Submit"),
  actionButton("new", "New"),
  actionButton("delete", "Delete")
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
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
}


# Shiny app with 3 fields that the user can submit data for
shinyApp(ui = ui, server = server)