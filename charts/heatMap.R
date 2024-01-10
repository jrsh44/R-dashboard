library(dplyr)
library(plotly)
library(jpeg)
library(png)
library(base64enc)


f_heat_map <-
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
    
    split_into_compartments <- function(vector, n) {
      compartment_range <- 14000/ (n-1)
      compartment_assignments <- numeric(length(vector))
      for (i in seq_along(vector)) {
        compartment_index <- as.integer((vector[i]) / compartment_range)+1
        compartment_assignments[i] <- compartment_index
      }
      return(compartment_assignments)
    }
    
    n <- 25
    
    data <-
      data %>% mutate(x_round = split_into_compartments(x, n),
                      y_round = split_into_compartments(y, n))
    
    hdata <- data %>% group_by(y_round,x_round) %>% 
      summarise(count = n()) %>% 
      filter(!is.na(x_round))
    m <- matrix(0, nrow = n, ncol = n)

    for (i in 1:nrow(hdata)) {
      m[n+1-as.numeric(hdata[i,2]),n+1-as.numeric(hdata[(i),1])] <- as.numeric(hdata[i,3])
    }
    
    palette <- colorRampPalette(c("darkblue","yellow","orange","red"))
    
    p <- plot_ly(
      z = m,
      colors = palette(5),
      type = "heatmap",
      opacity = 0.4,
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
      xaxis = list(showgrid = FALSE,showticklabels = FALSE,range(0,14000)), 
      yaxis = list(showgrid = FALSE,showticklabels = FALSE,range(0,14000))
      
    )
    
    return(p)
  }




f_heat_map(puuid_borycki,c("kill","death"),c("Orianna"),c("MIDDLE"),win=c(T,F),team_id = c(200,100))

