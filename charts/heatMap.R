# # For testing purposes only
# library(dplyr)
# library(plotly)
# library(jpeg)
# library(png)
# library(base64enc)
# puuid_cwalina <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_borycki <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_jarosz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

f_heat_map <- function(player,
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
    stop("Error: Invalid player.")
  }
    
    data <- df_participant_events %>% 
      dplyr::filter(player_id %in% player_puuid,
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
    
    data <- data %>% 
        mutate(x_round = split_into_compartments(x, n),
              y_round = split_into_compartments(y, n)) %>% 
        group_by(y_round,x_round) %>% 
        summarise(count = n()) %>% 
        filter(!is.na(x_round))
    m <- matrix(0, nrow = n, ncol = n)

    for (i in 1:nrow(data)) {
      m[n+1-as.numeric(data[i,2]),n+1-as.numeric(data[(i),1])] <- as.numeric(data[i,3])
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
        source = base64enc::dataURI(file = "./www/assets/rift2.jpeg"),
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

# f_heat_map("Borycki",c("kill","death"),c("Orianna"),c("MIDDLE"),win=c(T,F),team_id = c(200,100))
