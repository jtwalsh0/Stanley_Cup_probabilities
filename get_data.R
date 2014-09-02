library(rjson)
library(RCurl)
library(sqldf)



# ## BETFAIR
#   url <- getURL(url = 'https://www.kimonolabs.com/api/csv/4amybos2?apikey=13ff6ad50d64d091e0a23328afd5c04e')
# 
#   # Load the data
#   betfair <- read.csv(text = url, skip=1, header=TRUE)
# 
#   # Naive probabilities
#   betfair$naive_probability <- 1 / betfair$odds
# 
#   # Normalize so the probabilities add to 1
#   betfair$betfair.probability <- betfair$naive_probability / sum(betfair$naive_probability)



## BETONLINE
  url <- getURL(url = 'https://www.kimonolabs.com/api/csv/4d5njv8c?apikey=13ff6ad50d64d091e0a23328afd5c04e')

  # Load the data
  betonline <- read.csv(text = url, skip=1, header=TRUE)

  # Change names
  levels(betonline$team.text)[ levels(betonline$team.text) == "New York Islanders" ] <- "NY Islanders"
  levels(betonline$team.text)[ levels(betonline$team.text) == "New York Rangers" ] <- "NY Rangers"

  # Naive probabilities
  betonline$naive_probability <- 100 / (betonline$BetOnline + 100)
    betonline$naive_probability[ betonline$BetOnline < 0 ] <- betonline$BetOnline[ betonline$BetOnline < 0 ] / (betonline$BetOnline[ betonline$BetOnline < 0 ] - 100)

  # Normalize so the probabilities sum to 1
  betonline$BetOnline.probability <- betonline$naive_probability / sum(betonline$naive_probability)






## BODOG
  url <- getURL(url = 'https://www.kimonolabs.com/api/csv/d5e5i6q8?apikey=13ff6ad50d64d091e0a23328afd5c04e')

  # Load the data
  bodog <- read.csv(text = url, skip=1, header=TRUE)

  # Change names
  levels(bodog$team.text)[ levels(bodog$team.text) == "New York Islanders" ] <- "NY Islanders"
  levels(bodog$team.text)[ levels(bodog$team.text) == "New York Rangers" ] <- "NY Rangers"

  # Convert odds to naive probabilities
  bodog$naive.probability <- NA
  for(i in 1:30){
    temp <- as.numeric( strsplit(x=as.character(bodog$odds[i]), split="/")[[1]] )
    bodog$naive.probability[i] <- temp[2] / (temp[1] + temp[2])
  }

  # The naive probabilities sum to more than 1.  Remove the vig.
  bodog$bodog.probability <- bodog$naive.probability / sum( bodog$naive.probability )




## BOVADA         SAME AS BODOG
#   url <- getURL(url = 'https://www.kimonolabs.com/api/csv/dtzheweq?apikey=13ff6ad50d64d091e0a23328afd5c04e')
# 
#   # Load the data
#   bovada <- read.csv(text = url, skip=1, header=TRUE)
# 
#   # Change names
#   levels(bovada$team.text)[ levels(bovada$team.text) == "New York Islanders" ] <- "NY Islanders"
#   levels(bovada$team.text)[ levels(bovada$team.text) == "New York Rangers" ] <- "NY Rangers"
# 
#   # Convert odds to naive probabilities
#   bovada$naive.probability <- NA
#   for(i in 1:30){
#     temp <- as.numeric( strsplit(x=as.character(bovada$odds[i]), split="/")[[1]] )
#     bovada$naive.probability[i] <- temp[2] / (temp[1] + temp[2])
#   }
# 
#   # The naive probabilities sum to more than 1.  Remove the vig.
#   bovada$bovada.probability <- bovada$naive.probability / sum( bovada$naive.probability )





## GT Bets
  url <- getURL(url = 'https://www.kimonolabs.com/api/csv/5x9pged0?apikey=13ff6ad50d64d091e0a23328afd5c04e')

  # Load the data
  GTbets <- read.csv(text = url, skip=1, header=TRUE)

  # Change names
  levels(GTbets$team)[ levels(GTbets$team) == "New York Islanders" ] <- "NY Islanders"
  levels(GTbets$team)[ levels(GTbets$team) == "New York Rangers" ] <- "NY Rangers"

  # Extract the numbers needed to calculate the probabilities
  GTbets$odds <- as.character(GTbets$odds)
  GTbets$odds1 <- do.call(what = rbind,
                          args = strsplit(x = GTbets$odds, split = " to "))[,1]
  GTbets$odds2 <- do.call(what = rbind,
                          args = strsplit(x = GTbets$odds, split = " to "))[,2]
  GTbets$odds2 <- gsub(pattern = "\\)", replacement = "", x= GTbets$odds2)

  # Calculate naive probability (includes the vig)
  GTbets$naive.probability <- as.numeric(GTbets$odds2) / (as.numeric(GTbets$odds1) + as.numeric(GTbets$odds2))

  # Remove the vig
  GTbets$GTbets.probability <- GTbets$naive.probability / sum(GTbets$naive.probability)




## OddsShark
  url <- getURL(url = 'https://www.kimonolabs.com/api/csv/6pfkmri2?apikey=13ff6ad50d64d091e0a23328afd5c04e')

  # Load the data
  OddsShark <- read.csv(text = url, skip=1, header=TRUE)
    OddsShark <- na.omit(OddsShark)

  # Extract the numbers needed to calculate the probabilities
  OddsShark$FiveDimes.naive.probability <- 100 / (100 + OddsShark$FiveDimes)
    OddsShark$FiveDimes.naive.probability[ OddsShark$FiveDimes < 0 ] <- OddsShark$FiveDimes[ OddsShark$FiveDimes < 0 ] / (OddsShark$FiveDimes[ OddsShark$FiveDimes < 0 ] - 100)
  OddsShark$TopBet.naive.probability <- 100 / (100 + OddsShark$TopBet)
    OddsShark$TopBet.naive.probability[ OddsShark$TopBet < 0 ] <- abs(OddsShark$TopBet[ OddsShark$TopBet < 0 ]) / (100 + abs(OddsShark$TopBet[ OddsShark$TopBet < 0 ]))

  # Remove the vig
  OddsShark$FiveDimes.probability <- OddsShark$FiveDimes.naive.probability / sum(OddsShark$FiveDimes.naive.probability)
  OddsShark$TopBet.probability <- OddsShark$TopBet.naive.probability / sum(OddsShark$TopBet.naive.probability)




## SPORTSBOOK AG
  url <- getURL(url = 'https://www.kimonolabs.com/api/csv/d6upbwgc?apikey=13ff6ad50d64d091e0a23328afd5c04e')

  # Load the data
  sportsbook <- read.csv(text=url, skip=1, header=TRUE)

  # Change names
  levels(sportsbook$team)[ levels(sportsbook$team) == "New York Islanders" ] <- "NY Islanders"
  levels(sportsbook$team)[ levels(sportsbook$team) == "New York Rangers" ] <- "NY Rangers"

  # Convert moneyline to probability
  sportsbook$naive.probability <- 100 / (100 + sportsbook$odds.text)
    sportsbook$naive.probability[ sportsbook$odds.text < 0 ] <- sportsbook$odds.text[ sportsbook$odds.text < 0 ] / (sportsbook$odds.text[ sportsbook$odds.text < 0 ] - 100)

  # The naive probabilities sum to more than 1.  Remove the vig.
  sportsbook$sportsbook.probability <- sportsbook$naive.probability / sum( sportsbook$naive.probability )





## COMBINE PROBABILITIES INTO SINGLE OBJECT

data <- sqldf(x = " SELECT    OddsShark.team,
                              betonline_temp.betonline_probability,
                              bodog_temp.bodog_probability,
                              OddsShark.FiveDimes_probability,
                              GTbets_temp.GTbets_probability,
                              sportsbook_temp.sportsbook_probability,
                              OddsShark.TopBet_probability
                    FROM      OddsShark,
                              (SELECT   OddsShark.team as betonline_team,
                                        betonline.team_text,
                                        betonline.BetOnline_probability
                               FROM     OddsShark,
                                        betonline
                               GROUP BY OddsShark.team
                               ORDER BY MAX( DIFFERENCE(OddsShark.team, betonline.team_text) +
                                        DIFFERENCE( SOUNDEX(OddsShark.team), SOUNDEX(betonline.team_text) ) ) DESC) AS betonline_temp,
                              (SELECT   OddsShark.team as bodog_team,
                                        bodog.team_text,
                                        bodog.bodog_probability
                               FROM     OddsShark,
                                        bodog
                               GROUP BY OddsShark.team
                               ORDER BY MAX( DIFFERENCE(OddsShark.team, bodog.team_text) +
                                        DIFFERENCE( SOUNDEX(OddsShark.team), SOUNDEX(bodog.team_text) ) ) DESC) AS bodog_temp,
                              (SELECT   OddsShark.team as GTbets_team,
                                        GTbets.team,
                                        GTbets.GTbets_probability
                               FROM     OddsShark,
                                        GTbets
                               GROUP BY OddsShark.team
                               ORDER BY MAX( DIFFERENCE(OddsShark.team, GTbets.team) +
                                        DIFFERENCE( SOUNDEX(OddsShark.team), SOUNDEX(GTbets.team) ) ) DESC) AS GTbets_temp,
                              (SELECT   OddsShark.team as sportsbook_team,
                                        sportsbook.team,
                                        sportsbook.sportsbook_probability
                               FROM     OddsShark,
                                        sportsbook
                               GROUP BY OddsShark.team
                               ORDER BY MAX( DIFFERENCE(OddsShark.team, sportsbook.team) +
                                        DIFFERENCE( SOUNDEX(OddsShark.team), SOUNDEX(sportsbook.team) ) ) DESC) AS sportsbook_temp
                    WHERE     OddsShark.team = betonline_temp.betonline_team AND
                              OddsShark.team = bodog_temp.bodog_team AND
                              OddsShark.team = GTbets_temp.GTbets_team AND
                              OddsShark.team = sportsbook_temp.sportsbook_team",
              drv = "SQLite")


# Replace missing values with zeroes
data[ is.na(data) ] <- 0


# Ensemble   DON'T INCLUDE BETFAIR BECAUSE IT'S OFF
data$ensemble_probability <- rowMeans(x = data[,-1])



# Remove duplicates
data <- data[ !duplicated(data), ]



# Write data to CSV
write.csv(x = data, 
          file = "/home/jtwalsh0/ShinyApps/Stanley_Cup/market_data.csv", 
          append = TRUE,
          row.names = FALSE)



## RECORD TIME OF LAST UPDATES

  # Last Betfair update
  temp1 <- fromJSON(json_str=getURL('https://www.kimonolabs.com/api/4amybos2?apikey=13ff6ad50d64d091e0a23328afd5c04e'))$lastsuccess
  temp2 <- strptime(x = substr(temp1, start = 5, stop = 24),
                    format = "%b %d %Y %T",
                    tz = "GMT")
  last.betfair.update.time <- as.POSIXct(x = temp2, tz = "America/New_York")


  # Last Betonline update
  temp1 <- fromJSON(json_str=getURL('https://www.kimonolabs.com/api/4d5njv8c?apikey=13ff6ad50d64d091e0a23328afd5c04e'))$lastsuccess
  temp2 <- strptime(x = substr(temp1, start = 5, stop = 24),
                    format = "%b %d %Y %T",
                    tz = "GMT")
  last.betonline.update.time <- as.POSIXct(x = temp2, tz = "America/New_York")


  # Last Bodog update
  temp1 <- fromJSON(json_str = getURL('https://www.kimonolabs.com/api/d5e5i6q8?apikey=13ff6ad50d64d091e0a23328afd5c04e'))$lastsuccess
  temp2 <- strptime(x = substr(temp1, start = 5, stop = 24),
                    format = "%b %d %Y %T",
                    tz = "GMT")
  last.bodog.update.time <- as.POSIXct(x = temp2, tz = "America/New_York")


  # Last FiveDimes update
  temp1 <- fromJSON(json_str = getURL('https://www.kimonolabs.com/api/6pfkmri2?apikey=13ff6ad50d64d091e0a23328afd5c04e'))$lastsuccess
  temp2 <- strptime(x = substr(temp1, start = 5, stop = 24),
                    format = "%b %d %Y %T",
                    tz = "GMT")
  last.OddsShark.update.time <- as.POSIXct(x = temp2, tz = "America/New_York")


  # Last GTbets update
  temp1 <- fromJSON(json_str = getURL('https://www.kimonolabs.com/api/5x9pged0?apikey=13ff6ad50d64d091e0a23328afd5c04e'))$lastsuccess
  temp2 <- strptime(x = substr(temp1, start = 5, stop = 24),
                    format = "%b %d %Y %T",
                    tz = "GMT")
  last.GTbets.update.time <- as.POSIXct(x = temp2, tz = "America/New_York")


  # Last Sportsbook update
  temp1 <- fromJSON(json_str = getURL('https://www.kimonolabs.com/api/d6upbwgc?apikey=13ff6ad50d64d091e0a23328afd5c04e'))$lastsuccess
  temp2 <- strptime(x = substr(temp1, start = 5, stop = 24),
                    format = "%b %d %Y %T",
                    tz = "GMT")
  last.sportsbook.update.time <- as.POSIXct(x = temp2, tz = "America/New_York")


  # Save update times
  update.times <- data.frame(last.betfair.update.time,
                             last.betonline.update.time,
                             last.bodog.update.time,
                             last.OddsShark.update.time,
                             last.GTbets.update.time,
                             last.sportsbook.update.time)
  write.csv(x = update.times, 
            file = "/home/jtwalsh0/ShinyApps/Stanley_Cup/update_times.csv",
            row.names = FALSE)

