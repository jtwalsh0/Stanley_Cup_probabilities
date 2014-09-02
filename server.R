library(shiny)




shinyServer(function(input, output) {
    
  

  output$distPlot <- renderPlot({

    
    # Load data
    data <- read.csv("/home/jtwalsh0/ShinyApps/Stanley_Cup/market_data.csv", 
                     header = TRUE)

    
    # Identify maximum probability (so switching between estimates keeps same x axis)
    max.x <- max(data[,-1])
    

    
    # Data to use
    data$y <- data$ensemble_probability
    #if(input$select_box == 1)  data$y <- data$betfair_probability
    if(input$select_box == 2)  data$y <- data$BetOnline_probability
    if(input$select_box == 3)  data$y <- data$bodog_probability
    if(input$select_box == 4)  data$y <- data$FiveDimes_probability
    if(input$select_box == 5)  data$y <- data$GTbets_probability
    if(input$select_box == 6)  data$y <- data$sportsbook_probability
    if(input$select_box == 7)  data$y <- data$TopBet_probability
    if(input$select_box == 8)  data$y <- data$ensemble_probability
        
    
    
    # Order data
    data <- data[ order(data$y, decreasing=TRUE), ]

    
    # Rank
    data$rank <- 1:length(data$y)
    
    
    # Generate the graphic
    par(cex.axis=.9, cex.lab=.9, mar=c(4,4,2,0))
    plot(x = c(0.005, max.x+.02), 
         y = c(1, 30), 
         type='n', 
         axes=FALSE,
         xlab="probability of winning the 2015 Stanley Cup", 
         ylab="rank")
    box()
    axis(1)
    axis(3)
    axis(2, at=5:0*5+5, labels=0:5*5+1)
    
    points(x=data$y, y=6*5+1-data$rank, pch=20)
    text(x=data$y, y=6*5+1-data$rank, labels=data$team, pos=4, cex=.8)
    
  })
  
    
  
  # Return the time that each market was updated
  output$urlText <- renderText({
    
    update <- read.csv("/home/jtwalsh0/ShinyApps/Stanley_Cup/update_times.csv", header = TRUE)
    
    HTML(paste('<ul>',
      
               '<li> <a href="https://sports.yahoo.com/nhl/odds/futures">BetOnline (',
               as.character(update$last.betonline.update.time),
               ' ET)</a> </li>',
               
               '<li> <a href="http://sports.bodog.eu/sports-betting/hockey-futures.jsp">Bodog (',
               as.character(update$last.bodog.update.time),
               ' ET)</a> </li> ',
               
               '<li> <a href="http://www.oddsshark.com/nhl/odds/futures">5Dimes (',
               as.character(update$last.OddsShark.update.time),
               ' ET)</a> </li> ',
               
               '<li> <a href="http://www.gtbets.eu/betting1.asp?league=NHL&sport=NHL&specialeventname=2015+Stanley+Cup+-+To+Win&wagertype=FUTURE">GT Bets (',
               as.character(update$last.GTbets.update.time),
               ' ET)</a> </li> ',
               
               '<li> <a href="https://www.sportsbook.ag/sbk/sportsbook4/nhl-2015-nhl-futures-2015-stanley-cup-sports.sbk">Sportsbook (',
               as.character(update$last.sportsbook.update.time),
               ' ET)</a> </li> ',
               
               '<li> <a href="http://www.oddsshark.com/nhl/odds/futures">TopBet (',
               as.character(update$last.OddsShark.update.time),
               ' ET)</a> </li> ',
               
               '<li> <a href="http://www.scholarpedia.org/article/Ensemble_learning">Ensemble is the average of these sources</a> </li> ',
               '</ul>',
               sep = ''))
  })

  
})
