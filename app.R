##
## Shiny app for updating and monitoring a World Cup sweepstake
##

library(shiny)
library(shinyjs)
library(DBI)
library(ggplot2)
library(plotly)
source('lib.R')

# much of this code is from this blog:
# https://ipub.com/shiny-crud-app/



ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  titlePanel("FIFA 2022 Sweepstake"),
  
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
        tabPanel("Plots", plotOutput('scoresPlot', dblclick = "plot_dbclick"), plotlyOutput('cardsPlotly') ),
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
  
  # keep track of double-click event
  plot_click <- reactiveValues(trigger = 0)
  observe({
    req(input$plot_dbclick)
    isolate(plot_click$trigger <- plot_click$trigger + 1)
  })
  
  output$scoresPlotly <- renderPlotly({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    
    ts <- topScoringTeam()
    tc <- topConcedingTeam()
    maxScore = max(tc$MostGoals, ts$MostGoals)
    df = merge(ts,tc, by = 'Team', suffixes = c('.s','.c'))
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
    if (maxScore %% 2 == 1) {
      #maxScore = maxScore -1
    }
    tc$MostGoals <- tc$MostGoals * -1
    
    if (plot_click$trigger %% 2 == 0) {
      tc = tc[order(ts$MostGoals, decreasing = TRUE),]
      ts = ts[order(ts$MostGoals, decreasing = TRUE),]
    }
    
    # setting up a a pair of lined-up bar plots
    # top plot
    par(mfrow=c(2,1))
    par(mai=c(1,0.8,0,0))
    par(mgp=c(1.5,0.5,0.5))
    par(mar=c(0,3,3,0))
    # need to plot twice in order to have lines below bars
    barplot(rep(NA, length(ts$MostGoals)), ylim=c(0,maxScore), axes = FALSE)
    abline(h=seq(0,10,2), col='lightgray')
    barplot(ts$MostGoals, col="darkgreen", add = TRUE, yaxt = 'n')
    # ensure axis labels are integers
    axis(2, at = 0:maxScore, las=1, tck=-0.02, cex.lab = 0.5, cex.axis = 0.8)
    # bottom plot
    par(mai=c(1,0.8,0,0))
    par(mar=c(5,3,0,0))
    barplot(rep(NA, length(tc$MostGoals)), ylim=c(-maxScore,0), axes = FALSE)
    abline(h=seq(-10,0,2), col='lightgray')
    barplot(tc$MostGoals, 
            names.arg = tc$Team, 
            col="red", 
            ylab='Conceeded', 
            add = TRUE,
            yaxt = 'n')
    axis(2, at = 0:-maxScore, las=1, tck=-0.02, cex.names = 0.5, cex.axis = 0.8, )
    


  })
  
  output$cardsPlotly <- renderPlotly({
    df = cardsPerGame()
    fig <- plot_ly(df,
      x = ~Team,
      y = ~cpg,
      type = "bar",
      marker = list(color = 'yellow',
                    line = list(color = 'rgb(8,48,107)', width = 1.5))
    )
    fig %>% layout(
      yaxis = list(title = "Cards per Game")
    )

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