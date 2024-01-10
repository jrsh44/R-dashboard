library(dplyr)
library(plotly)
library(jpeg)
library(png)
library(base64enc)

f_map <-
  function(player_puuid,
           stats,
           champion_names,
           positions,
           wins,
           team_ids) {
    # stats - kill, death or assist (string)
    # champion_name and positions can be vectors length >=1 (string)
    # win - TRUE, FALSE, c(TRUE,FALSE)
    # team_id - c(100,200)
    
    data <- read.csv("./db/participantEvents.csv")
    
    if (player_puuid == puuid_cwalina) {
      player_name <- "Janek"
    } else if (player_puuid == puuid_borycki) {
      player_name <- "Bartek"
    } else if (player_puuid == puuid_jarosz) {
      player_name <- "Mateusz"
    } else {
      stop("Error: Invalid player_puuid.")
    }
     
    data <- data %>% 
      filter(player_id %in% player_puuid,
      type %in% stats,
      position %in% positions,
      champion_name %in% champion_names,
      win %in% wins,
      team_id %in% team_ids)
    
    plot <- data %>% plot_ly(
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers",
      color = ~type,
      size = 3,
      height = 416,
      width = 500
    ) %>% layout(
      images = list(
        source = base64enc::dataURI(file = "./rift2.jpeg"),
        x = 0,
        y = 0,
        sizex = 1,
        sizey = 1,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        layer = "below"
      ),
      xaxis = list(showgrid = FALSE,showticklabels = FALSE,title='',range(0,14000)), 
      yaxis = list(showgrid = FALSE,showticklabels = FALSE,title = "",range(0,14000))
    )
    return(plot)
  }





f_map(puuid_borycki,c("death","kill","assist"),c("Shaco"),c("JUNGLE"),win=c(T,F),team_id = c(200,100))

