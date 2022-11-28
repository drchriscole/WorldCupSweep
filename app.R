##
## Shiny app for updating and monitoring a World Cup sweepstake
##

library(shiny)
library(shinyjs)
library(shinydashboard)
library(DBI)
library(ggplot2)
library(plotly)
library(dplyr)
source('lib.R')

# much of this code is from this blog:
# https://ipub.com/shiny-crud-app/



ui <- dashboardPage(
  dashboardHeader(title = "FIFA 2022 Sweepstake"), 
  dashboardSidebar(collapsed = TRUE,
       #use shiny js to disable the ID field
       shinyjs::useShinyjs(),
        
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
    
    dashboardBody(
       tabsetPanel(type = "tabs",
           tabPanel("Plots", 
              fluidRow(
                infoBoxOutput("topScoreTeam"),
                infoBoxOutput("topConcedingTeam"),
                infoBoxOutput("leastDiscTeam")
              ),
              fluidRow(
                box(title = "Goals Scored/Conceded", width = 12, 
                    solidHeader = TRUE, background = 'light-blue',
                    plotOutput('scoresPlot', dblclick = "plot_dbclick")
                )
              ),
              fluidRow(
                box(title = "Least Disciplined Teams", width = 12, 
                    solidHeader = TRUE, background = 'light-blue',
                    plotlyOutput('cardsPlotly') 
                )
              )
           ),
           tabPanel("Table", 
              DT::dataTableOutput("responses")
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
  
  output$topScoreTeam <- renderInfoBox({
    ts <- topScoringTeam()
    top = ts[order(ts$MostGoals, decreasing = TRUE), 1]
    team = top[1]
    
    infoBox(title = 'Best Scoring Team', 
            value = names(teams)[teams == team],
            icon = icon('futbol'),
            color = 'olive'
    )
  })

  output$topConcedingTeam <- renderInfoBox({
    tc <- topConcedingTeam()
    top = tc[order(tc$MostGoals, decreasing = TRUE), 1]
    team = top[1]
    
    infoBox(title = 'Most Conceding Team', 
            value = names(teams)[teams == team],
            icon = icon('futbol'),
            color = 'red'
    )
  })
  
  output$leastDiscTeam <- renderInfoBox({
    df = cardsPerGame()
    top = df[order(df$cpg, decreasing = TRUE), 1]
    team = top[1]
    infoBox(title = 'Least Disciplined Team', 
            value = names(teams)[teams == team],
            icon = icon('exclamation'),
            color = 'yellow'
    )
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
    
    # get scoring data and merge
    ts <- topScoringTeam()
    ts$Type = 'For'
    tc <- topConcedingTeam()
    tc$Type = 'Against'
    tc$MostGoals = tc$MostGoals * -1
    df = rbind(ts, tc)
    
    max.score = max(abs(df$MostGoals))
    
    # this looks more complicated than it should be, but
    # find the team order based on most scored or conceded goals
    team.order = df %>% 
      group_by(Team) %>% 
      slice_max(abs(MostGoals), with_ties = FALSE) %>% 
      arrange(desc(MostGoals)) %>%
      select(Team) %>%
      pull()
    
    # change sort order if double-clicked
    if (plot_click$trigger %% 2 == 0) {
      df$Team = factor(df$Team, levels = team.order)
    }
    
    ggplot(df, aes(x = Team, y = MostGoals, fill = Type)) + 
      labs(x = 'Team',
           y = 'Goals Scored (For/Against)',
           fill = '') +
      ylim(c(-1*max.score, max.score)) +
      geom_col() + 
      # add score to bar and remove zeros
      # geom_text(aes(label = ifelse(abs(MostGoals) > 0, MostGoals, NA )),
      #           colour = 'white', 
      #           fontface = 'bold',
      #           nudge_y = ifelse(df$MostGoals < 0, 0.3, -0.3)) +
      scale_fill_manual(values = c('red','darkgreen')) +
      theme_minimal() +
      theme(legend.position = 'top')
    
    
    
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