library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  h1("Stanley Cup Probabilities", align="center"),
  
  # Let user choose the plot
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="select_box", 
                  label = h3("Estimates to Display"), 
                  choices = list(#"Betfair" = 1, 
                                 "BetOnline" = 2, 
                                 "Bodog" = 3, 
                                 "5Dimes" = 4,
                                 "GT Bets" = 5,
                                 "Sportsbook" = 6,
                                 "TopBet" = 7,
                                 "Ensemble" = 8), 
                  selected = 8),
      h6("Sources (time last updated):"),
      htmlOutput("urlText")
    ),
    mainPanel( 
      plotOutput("distPlot") 
    )
  )
))

