# # For testing purposes only
# library(dplyr)
# library(plotly)
# library(jpeg)
# library(png)
# library(base64enc)
# df_participant_events <- read.csv("./db/participantEvents.csv")
# puuid_cwalina <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_borycki <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_jarosz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

f_map <- function(player,
          stats,
          champion_names,
          positions,
          wins,
          team_ids) {
    # stats - kill, death or assist (string)
    # champion_name and positions can be vectors length >=1 (string)
    # win - TRUE, FALSE, c(TRUE,FALSE)
    # team_id - c(100,200)
        
  if (player == "Cwalina") {
    player_puuid <- puuid_cwalina
  } else if (player == "Borycki") {
    player_puuid <- puuid_borycki
  } else if (player == "Jarosz") {
    player_puuid <- puuid_jarosz
  } else {
    stop("Error: Invalid player_puuid.")
  }
     
    data <- df_participant_events %>% 
      dplyr::filter(player_id %in% player_puuid,
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
        source = base64enc::dataURI(file = "./assets/rift2.jpeg"),
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

# f_map(puuid_borycki,c("death","kill","assist"),c("Shaco"),c("JUNGLE"),win=c(T,F),team_id = c(200,100))
