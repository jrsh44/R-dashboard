# # For testing purposes only
# library(dplyr)
# library(plotly)
# library(lubridate)
# df_matches <- read.csv("./db/matches.csv")
# puuid_cwalina <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_borycki <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_jarosz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

f_plot_time <- function(player){

    # Player names and colors
    if(player == "Cwalina"){
        player_puuid <- puuid_cwalina
        color <- "#4F6F52"
    } else if (player == "Borycki"){
        player_puuid <- puuid_borycki
        color <- "#c99b3f"
    } else if (player == "Jarosz"){
        player_puuid <- puuid_jarosz
        color <- "#005a82"
    } else {
        stop("Error: Invalid player_puuid.")
    }

    # Proccess data
    player_data <- df_matches %>%
        dplyr::filter(paricipants == player_puuid) %>%
        mutate(day_of_week = wday(game_creation, label = TRUE, abbr = FALSE)) %>%
        mutate(hour = hour(game_creation)) %>%
        group_by(day_of_week, hour) %>%
        summarise(gamesPlayed = n())

    all_combinations <- expand.grid(
        day_of_week = unique(player_data$day_of_week),
        hour = 0:23
        )
    
    player_data <- player_data %>%
        full_join(all_combinations, by = c("day_of_week", "hour"))

    color_scale <- list(
        c(0, "#f0e6d2"),
        c(1, color)
    )

    # Create plot
    plot <- plot_ly(
        data = player_data,
        y = ~day_of_week,
        x = ~hour,
        z = ~gamesPlayed, 
        colorscale = color_scale,
        type = "heatmap",
        showscale = FALSE,
        opacity = 0.85) %>%
    layout(
        xaxis = list(
            title = "Godzina rozpoczęcia gry", 
            dtick = 2, 
            range = c(-0.5, 23.5), 
            showgrid = TRUE, 
            gridcolor = "#c8aa6e35"
        ), 
        yaxis = list(
            title = "", 
            showgrid = TRUE, 
            gridcolor = "#c8aa6e35", 
            tickfont = list(size = 14)
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(
            size = 14,
            color = "#c8aa6e"
        ),
        showlegend = FALSE
    ) %>%
    config(displayModeBar = FALSE, staticPlot = TRUE)

    return(plot)
}

# f_plot_time(puuid_borycki)