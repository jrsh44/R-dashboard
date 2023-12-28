library(plotly)
library(dplyr)

f_plot_champions <- function(player_puuid, stat, champ_amount = 10){
    data <- read.csv("./db/playerMatchStats.csv")

    # Player names and colors
    if(player_puuid == puuid_cwalina){
        player_name <- "Janek"
        color <- "#4F6F52"
    } else if (player_puuid == puuid_borycki){
        player_name <- "Bartek"
        color <- "#F6C86B"
    } else if (player_puuid == puuid_jarosz){
        player_name <- "Mateusz"
        color <- "#86B6F6"
    } else {
        stop("Error: Invalid player_puuid.")
    }

    # Filter data
    player_data <- data %>%
        filter(player_id == player_puuid)

    # Filter out ${champ_amount} champions that were played the most
    top_champions <- player_data %>%
        group_by(champion_name) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(champ_amount) %>%
        pull(champion_name)

    player_data <- player_data %>%
        filter(champion_name %in% top_champions)

    # Prepare stat to plot and set y axis title
    if (stat == "kills") {
        y_axis_title <- "Ilość zabójstw"
        stats <- player_data %>% 
            group_by(champion_name) %>% 
            summarise(kills = sum(kills)) 
    } else if (stat == "deaths") {
        y_axis_title <- "Ilość śmierci"
        stats <- player_data %>% 
            group_by(champion_name) %>% 
            summarise(deaths = sum(deaths))
    } else if (stat == "kda"){
        y_axis_title <- "Współczynnik zabójstw, śmierci i asyst"
        stats <- player_data %>%
            group_by(champion_name) %>%
            summarise(kda = (sum(kills) + sum(assists)) / sum(deaths))
    } else if (stat == "winrate"){
        y_axis_title <- "Procent wygranych gier"
        stats <- player_data %>%
            group_by(champion_name) %>%
            summarise(winrate = sum(win)/n())
    } else if (stat == "gamesPlayed"){
        y_axis_title <- "Ilość rozegranych gier"
        stats <- player_data %>%
            group_by(champion_name) %>%
            summarise(gamesPlayed = n())
    } else {
        stop("Error: Invalid stat.")
    }

    # Plot
    plot_champions <- stats %>%
        plot_ly(
            x = ~factor(champion_name), 
            y = ~get(stat), 
            type = 'bar',
            marker = list(color = paste(color, '88', sep=""), line = list(color = color, width = 2))) %>%
        layout(
            xaxis = list(title = "Nazwa cheampiona"),
            yaxis = list(title = y_axis_title))

    return(plot_champions)
}

# Possible values: "kills", "deaths", "kda", "winrate", "gamesPlayed"
f_plot_champions(puuid_jarosz, "kda")
