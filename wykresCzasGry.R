library(httr)
library(jsonlite)s
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)



matchidsBartek <- get_match_ids(api_key, puuidBartek)
matchListBartek <- get_matches(api_key, matchidsBartek, 1, 100)
matchidsJanek <- get_match_ids(api_key, puuidJanek)
matchListJanek <- get_matches(api_key, matchidsJanek, 1, 100)
matchidsJarosz <- get_match_ids(api_key, puuidJarosz)
matchListJarosz <- get_matches(api_key, matchidsJarosz, 1, 100)





match_time_df <- function(matches,playerid) {
  # czyjemecze - string z imieniem
  df <-
    data.frame(
      player_id = vector("character", length = length(matches)),
      match_id = vector("character", length = length(matches)),
      date = vector("double", length = length(matches)),
      year = vector("double", length = length(matches)),
      month = vector("character", length = length(matches)),
      day = vector("character", length = length(matches)),
      day_of_week = vector("character", length = length(matches)),
      hour = vector("character", length = length(matches)),
      minute = vector("character", length = length(matches)),
      second = vector("character", length = length(matches)))
  
  for (i in 1:length(matches)) {
    df$player_id[i] <- playerid
    df$match_id[i] <- matches[[i]]$metadata$matchId
    df$date[i] <- as.character(as.POSIXct(matches[[i]]$info$gameCreation/1000,origin = "1970-01-01", tz = "UTC"))
    
  }
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S",origin = "1970-01-01", tz = "UTC")
  df$year <- year(df$date)
  df$month <- month(df$date)
  df$day <- day(df$date)
  df$day_of_week <- wday(df$date)
  df$hour <- hour(df$date)
  df$minute <- minute(df$date)
  df$second <- second(df$date)
  
  return(df)
}

df <- match_time_df(matchListBartek,'Bartek')
df <- rbind(df,match_time_df(matchListJanek,'Janek'))
df <- rbind(df,match_time_df(matchListJarosz,'Mateusz'))


plot <- df %>% 
  plot_ly(y = ~day_of_week,x= ~hour, type = "box", color = ~as.factor(player_id), orientation = "h") %>% 
  layout(
    boxmode = "group",
    title="Czas gry",
    yaxis = list(title="Dzień tygodnia",range = unique(df$day_of_week)),  
    xaxis = list(title="Godzina rozpoczęcia gry",range = c(0, 24)),
    uirevision = "lock"
  )


plot
