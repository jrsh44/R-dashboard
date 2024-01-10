library(dplyr)
library(plotly)
library(jpeg)
library(base64enc)


f_animated_map <-
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
  
    #View(data %>% group_by(minute,type) %>% summarize(n=n()))
    plot <- data %>% plot_ly(
      height = 500,
      width = 420,
      type = "scatter",
      mode = "markers"
    ) %>%  add_markers(x = ~x,
                  y = ~y,
                  frame = ~minute,
                  size = 3
                  )%>% 
      animation_opts(frame = 2000,
                     transition=600) %>% layout(
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
      xaxis = list(showgrid = F,showticklabels = F,title='',range(0,14000)), 
      yaxis = list(showgrid =F,showticklabels =F,title = "",range(0,14000)),
      showlegend = FALSE
    )
    return(plot)
  }





f_animated_map(puuid_borycki,c("death","kill"),c("Orianna"),c("MIDDLE"),win=c(T,F),team_id = c(200,100))

