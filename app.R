##
## Shiny app for updating and monitoring a World Cup sweepstake
##

library(shiny)
library(shinyjs)
library(DBI)
library(ggplot2)
source('lib.R')

# much of this code is from this blog:
# https://ipub.com/shiny-crud-app/

con = getCon()

ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  titlePanel("Euro 2020 Sweepstake"),
  
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
        tabPanel("Plots", plotOutput('scoresPlot', click = "plot_click"), plotOutput('cardsPlot') ),
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
      UpdateData(formData(), con)
    } else {
      CreateData(formData(), con)
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData(), con)
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData(con)[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display plot
  output$scoresPlot <- renderPlot({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ts <- topScoringTeam(con)
    ts$Team <- factor(ts$Team, levels=ts$Team)
    tc <- topConcedingTeam(con)
    tc$Team <- factor(tc$Team, levels=tc$Team)
    maxScore = max(tc$MostGoals, ts$MostGoals)
    if (maxScore %% 2 == 1) {
      maxScore = maxScore -1
    }
    tc$MostGoals <- tc$MostGoals * -1
    
    if (!is.null(input$plot_click)) {
      tc = tc[order(ts$MostGoals, decreasing = TRUE),]
      ts = ts[order(ts$MostGoals, decreasing = TRUE),]
    }
    
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
    df = cardsPerGame(con)
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
    ReadData(con)
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