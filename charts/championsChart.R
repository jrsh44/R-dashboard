# # For testing purposes only
# library(plotly)
# library(dplyr)
# df_player_match_stats <- read.csv("./db/playerMatchStats.csv")
# puuid_cwalina <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_borycki <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_jarosz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

f_plot_champions <- function(player, stat, champ_amount = 10){

    # Player names and colors
    if(player == "Cwalina"){
        player_puuid <- puuid_cwalina
        color <- "#4F6F52"
    } else if (player == "Borycki"){
        player_puuid <- puuid_borycki
        color <- "#F6C86B"
    } else if (player == "Jarosz"){
        player_puuid <- puuid_jarosz
        color <- "#86B6F6"
    } else {
        stop("Error: Invalid player_puuid.")
    }

    # Filter data
    player_data <- df_player_match_stats %>%
        dplyr::filter(player_id == player_puuid)

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
            mutate(deaths = ifelse(deaths == 0, 1, deaths)) %>%
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
            marker = list(color = paste(color, '88', sep=""), 
            line = list(color = color, width = 2))) %>%
        layout(
            xaxis = list(title = "Nazwa cheampiona"),
            yaxis = list(title = y_axis_title))

    return(plot_champions)
}

# # # Possible values: "kills", "deaths", "kda", "winrate", "gamesPlayed"
# f_plot_champions("Jarosz", "kda")
