library(dplyr)
library(plotly)
library(lubridate)


f_plot_time <- function(player_puuid){
    data <- read.csv("./db/matches.csv")

    # Player names and colors
    if(player_puuid == puuid_cwalina){
        player_name <- "Janek"
        color <- "#4F6F52"
    } else if (player_puuid == puuid_borycki){
        player_name <- "Bartek"
        color <- "#9B1D20"
    } else if (player_puuid == puuid_jarosz){
        player_name <- "Mateusz"
        color <- "#86B6F6"
    } else {
        stop("Error: Invalid player_puuid.")
    }

    # Proccess data
    player_data <- data %>%
        filter(paricipants == player_puuid) %>%
        mutate(day_of_week = wday(game_creation, label = TRUE, abbr = FALSE)) %>%
        mutate(hour = hour(game_creation)) %>%
        group_by(day_of_week, hour) %>%
        summarise(gamesPlayed = n())

    all_combinations <- expand.grid(
        day_of_week = unique(player_data$day_of_week),
        hour = 0:23
        )
    
    player_data <- player_data %>%
        full_join(all_combinations, by = c("day_of_week", "hour")) %>%
        mutate(gamesPlayed = ifelse(is.na(gamesPlayed), 0, gamesPlayed))

    color_scale <- list(
        c(0, "#FFFFFF"),
        c(0.01, "#e2e2e2"),
        c(1, color)
    )

    # Create plot
    plot <- plot_ly(
        data = player_data,
        y = ~day_of_week,
        x = ~hour,
        z = ~gamesPlayed, 
        colorscale = color_scale,
        type = "heatmap") %>%
    layout(
            xaxis = list(title = "Godzina rozpoczęcia gry", dtick = 2, range = c(0, 23), showline = TRUE, linecolor = 'black'), 
            yaxis = list(title = "", showline = TRUE, linecolor = 'black'))

    return(plot)
}

f_plot_time(puuid_jarosz)