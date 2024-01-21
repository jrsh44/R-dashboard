library(jpeg)
library(png)
library(shinythemes)
library(base64enc)
library(httr)
library(networkD3)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shiny)

source("./db/variables.R")
source("./db/items.R")
source("./charts/animatedMap.R")
source("./charts/championsChart.R")
source("./charts/heatMap.R")
source("./charts/itemsSankeyGraph.R")
source("./charts/mapPlot.R")
source("./charts/playerOverallStatsChart.R")
source("./charts/timePlayChart.R")

ui <- navbarPage(
  title = "League of Legends - stats",
  
  # Zakładka 1
  tabPanel(
    "Ogólne",
    tags$head(
      tags$title("League of Legends Stats"),
      tags$link(rel = "icon", href = "assets/favicon.png"),
      tags$link(rel = "stylesheet", href = "fonts.css"),
      tags$link(rel = "stylesheet", href = "styles.css"),
    ),
    tags$div(
      class = "tab-wrapper",
      tags$div(
        class = "tab-title-text",
        tags$div(class = "typoH1", "Ogólne statystyki"),
        tags$img(class = "tab-decorator-lg", src = "assets/decorator-hr-lg.png")
      ),
      tags$div(
        class = "tab-champ-container",
        tags$div(
          class = "tab-champ-wrapper",
          tags$div(
            class = "tab-title-text",
            tags$div(class = "typoH2", "Porównanie championów"),
            tags$div(class = "typoH5", "którymi gramy"),
            tags$img(class = "tab-decorator", src = "assets/decorator-hr.png"),
          ),
          tags$div(
            class = "typoBodyBold",
            "Obecnie w grze dostępnych jest ponad 150 postaci podzielonych na różne kategorie, takie jak zabójcy, strażnicy, magowie czy strzelcy. Stojąc przed takim szerokim wyborem istotne staje się określenie, którym bohaterem radzimy sobie najlepiej,"
          ),
          tags$div(class = "typoBody", textOutput("t1_stat_desc")),
          tags$div(
            class = "tab-champ-buttons",
            selectInput(
              inputId = "t1_player",
              label = "Gracz:",
              choices = c("Cwalina", "Borycki", "Jarosz")
            ),
            selectInput(
              inputId = "t1_stat",
              label = "Statystyka:",
              choices = c(
                "Zabójstwa",
                "Śmierci",
                "Współczynnik KDA",
                "Współczynnik zwycięstw",
                "Liczba gier"
              )
            ),
          ),
          tags$div(class = "tab1-champ-wrapper",
            plotlyOutput("t1_champions_chart"),
          ),
        ),
        tags$div(class = "tab1-time-container",
          tags$div(class = "tab1-title-text",
            tags$div(class = "typoH2", "Kiedy najczęściej gramy"),
            tags$img(class = "tab1-decorator", src = "assets/decorator-hr.png"),
            ),
          tags$div(class = "tab1-time-desc-wrapper",
            tags$div(class = "tab1-time-text-wrapper", 
              tags$div(class = "typoBodyBold", "Każdy z nas ma wiele różnych obowiązków takich jak studia czy praca, ale łączy nas fakt, że zawsze znajdziemy czas, aby pograć w Ligę. Poniższa heatmapa prezentuje kiedy najczęściej zdarza nam się grać w przeciągu całego tygodnia.")
            ),
            tags$div(class = "tab1-time-button-wrapper", 
              selectInput(inputId = "t1_player_time",label = "Gracz:", choices = c("Cwalina","Borycki","Jarosz")),
            ),
          ),
          tags$div(class = "tab1-time-wrapper",
            plotlyOutput("t1_time_played_chart")
          ),
        )
     )
  ),

  #Zakładka 2
  tabPanel(
    "Mapy",
    tags$head(),
    tags$div(
      class = "tab-wrapper",
      tags$div(
        class = "tab-title-text",
        tags$div(class = "typoH1", "Przebieg gier"),
        tags$img(class = "tab-decorator-lg", src = "assets/decorator-hr-lg.png")
      ),
      tags$div(
        class = "tab-map-container",
        tags$div(
          class = "tab-map-info-wrapper",
          tags$div(
            class = "tab-title-text",
            tags$div(class = "typoH2", "Aktywność na mapie"),
            tags$div(class = "typoH5", "w zależności od pozycji"),
            tags$img(class = "tab-decorator", src = "assets/decorator-hr.png"),
          ),
          tags$div(
            class = "typoBodyBold",
            "Podczas rozgrywki, każdy gracz jest przypisany do jednej z pięciu pozycji. Zobaczmy, jak wygląda nasza aktywność, mierzona w liczbe zabójstw, śmierci i asyst, w różnych częściach mapy, w zależności od pozycji na której gramy"
          ),
          tags$div(class = "typoBody", textOutput("t2_desc")),
          tags$div(
            class = "tab-map-buttons",
            selectInput(
                      inputId = "t2_map_type",
                      label = "Typ:",
                      choices = c("Heatmap", "Scatter", "Animated"),
                      selected = "Heatmap"
                    ),
            selectInput(
                      inputId = "t2_map_player",
                      label = "Summoner:",
                      choices = c("Cwalina", "Borycki", "Jarosz"),
                      selected = "Borycki"
                    ),
            uiOutput("t2_dynamic_input")
            ),
          ),
        tags$div(class = "tab-map-wrapper",
                 plotlyOutput("t2_map"), ),
        ),
      )
    ),
  
  # Zakładka 3
  tabPanel(
    "Szczegółowe",
    tags$div(class = "typoH1", "Statystyki szczegółowe"),
    fluidRow(
      column(
        4,
        selectInput(
          inputId = "summoner",
          label = "Summoner:",
          choices = c("Cwalina", "Borycki", "Jarosz")
        ),
        selectInput(
          inputId = "Position",
          label = "Position",
          choices = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
          selected = "BOTTOM"
        ),
        uiOutput("t3_dynamic_input"),
        uiOutput("t3_dynamic_input2"),
        selectInput(
          inputId = "type",
          label = "Type:",
          choices = c("Density", "Chronologically")
        )
      ),
      column(
        4,
        plotlyOutput("t3_dmg_per_death"),
        plotlyOutput("t3_minions_per_minute")
      ),
      column(
        4,
        plotlyOutput("t3_kill_participation"),
        plotlyOutput("t3_kda")
      )
    ),
    fluidRow(
      titlePanel("Itemki"),
      selectInput(
        inputId = "sankey_summoner",
        label = "Summoner:",
        choices = c("Cwalina", "Borycki", "Jarosz")
      ),
      sankeyNetworkOutput("t3_sankey"),
      plotOutput("t3_sankey_legend")
    )
  ),
  
  # ABOUT PAGE
  tabPanel(
    "About",
    tags$div(
      class = "about-container",
      tags$div(class = "typoH5", "League of Legends - Analise"),
      tags$div(class = "typoH1", "Informacje o projekcie"),
      tags$img(class = "tab4-decorator-lg", src = "assets/decorator-hr-lg.png"),
    )
  )
)

server <- function(input, output) {
  # Zakładka 1
  output$t1_stat_desc <- renderText({
    if (input$t1_stat == "Zabójstwa") {
      "Liczba zabójstwa informuje o tym, ilu przeciwników udało nam się pokonać podczas rozgrywki. Im więcej zabójstw, tym silniejszy mamy wpływ na przebieg gry."
    } else if (input$t1_stat == "Śmierci") {
      "Liczba śmierci informuje o tym, ile razy bohater został pokonany przez przeciwników."
    } else if (input$t1_stat == "Współczynnik KDA") {
      "Współczynnik KDA to suma zabójstw i asyst podzielona przez liczbę śmierci. Jest to miara efektywności gracza w walce."
    } else if (input$t1_stat == "Współczynnik zwycięstw") {
      "Współczynnik zwycięstw odzwierciedla procent wygranych gier spośród wszystkich rozegranych."
    } else if (input$t1_stat == "Liczba gier") {
      "Liczba gier pozwala określić nie tylko popularność postaci, ale również doświadczenie gracza w jej obsłudze."
    }
  })
  
  output$t1_champions_chart <- renderPlotly({
    f_plot_champions(input$t1_player, input$t1_stat)
  })
  
  output$t1_time_played_chart <- renderPlotly({
    f_plot_time(input$t1_player_time)
  })
  
  
  # Zakładka 2
  output$t2_desc <- renderText({
    if(input$t2_map_type == "Animated"){
      "Animacja pokazuje naszą aktywność na mapie w każdej minucie z osobna, co wizualizuje, jak zmienia się dynamika naszej rozgrywki w czasie"
    }else{""}
  })
  output$t2_dynamic_input <- renderUI({
    myStatsPosition <- df_item_champ %>%
      dplyr::filter(player_id == as.vector(
        summoner %>% filter(name %in% input$t2_map_player) %>% select(puuid)
      ))
    most_played <-
      myStatsPosition %>% group_by(position) %>%
      summarise(n = n()) %>% slice_max(order_by = n, n = 1) %>%
      select(position) %>% pull()
    selectInput(
      inputId = "t2_position",
      label = "Pozycja:",
      choices = c(unique(unlist(
        myStatsPosition$position
      ))),
      selected = most_played
    )
    
  })
  
  output$t2_map <- renderPlotly({
    if (input$t2_map_type == "Animated") {
      f_animated_map(
        input$t2_map_player,
        c("death", "kill", "assist"),
        "All",
        input$t2_position,
        win = c(T, F),
        team_id = c(200, 100)
      )
    } else if (input$t2_map_type == "Scatter") {
      f_map(
        input$t2_map_player,
        c("death", "kill", "assist"),
        "All",
        input$t2_position,
        win = c(T, F),
        team_id = c(200, 100)
      )
    } else{
      f_heat_map(
        input$t2_map_player,
        c("death", "kill", "assist"),
        "All",
        input$t2_position,
        win = c(T, F),
        team_id = c(200, 100)
      )
    }
  })
  
  
  # Zakładka 3
  
  output$t3_dynamic_input <- renderUI({
    myStatsPosition <- df_item_champ %>%
      dplyr::filter(
        player_id == as.vector(
          summoner %>% filter(name %in% input$summoner) %>% select(puuid)
        ),
        position == input$Position
      )
    if (nrow(myStatsPosition) == 0) {
      selectInput(inputId = "id1",
                  label = "Champion",
                  choices = c("None"))
    } else {
      selectInput(
        inputId = "id1",
        label = "Champion",
        choices = c("All", unique(
          unlist(myStatsPosition$champion_name)
        ))
      )
    }
  })
  
  output$t3_dynamic_input2 <- renderUI({
    if (input$type == "Density") {
      myStatsPosition <- df_item_champ %>%
        filter(
          player_id == as.vector(
            summoner %>% filter(name %in% input$summoner) %>% select(puuid)
          ),
          position == input$Position
        )
      if (nrow(myStatsPosition) == 0) {
        selectInput(
          inputId = "compare",
          label = "Compare with:",
          choices = c("None")
        )
      } else {
        champs <- unique(unlist(myStatsPosition$champion_name))
        if (input$id1 == "All" | input$id1 == "None") {
          selectInput(
            inputId = "compare",
            label = "Compare with:",
            choices = c("Don't")
          )
        } else {
          selectInput(
            inputId = "compare",
            label = "Compare with:",
            choices = c("Don't", "All", champs[champs != input$id1])
          )
        }
      }
    }
  })
  
  output$t3_dmg_per_death <- renderPlotly({
    if (input$type == 'Density') {
      f_overall_stats_plot(
        input$summoner,
        input$Position,
        input$id1,
        input$type,
        "DmgPerDeath",
        input$compare
      )
    } else {
      f_overall_stats_plot(input$summoner,
                           input$Position,
                           input$id1,
                           input$type,
                           "DmgPerDeath")
    }
  })
  
  output$t3_kill_participation <- renderPlotly({
    if (input$type == 'Density') {
      f_overall_stats_plot(
        input$summoner,
        input$Position,
        input$id1,
        input$type,
        "kill_participation",
        input$compare
      )
    } else {
      f_overall_stats_plot(input$summoner,
                           input$Position,
                           input$id1,
                           input$type,
                           "kill_participation")
    }
  })
  
  output$t3_minions_per_minute <- renderPlotly({
    if (input$type == 'Density') {
      f_overall_stats_plot(
        input$summoner,
        input$Position,
        input$id1,
        input$type,
        "MinionsPerMinute",
        input$compare
      )
    } else {
      f_overall_stats_plot(input$summoner,
                           input$Position,
                           input$id1,
                           input$type,
                           "MinionsPerMinute")
    }
  })
  
  output$t3_kda <- renderPlotly({
    if (input$type == 'Density') {
      f_overall_stats_plot(input$summoner,
                           input$Position,
                           input$id1,
                           input$type,
                           "kda",
                           input$compare)
    } else {
      f_overall_stats_plot(input$summoner,
                           input$Position,
                           input$id1,
                           input$type,
                           "kda")
    }
  })
  
  output$t3_sankey <- renderSankeyNetwork({
    f_items_sankey_graph(input$sankey_summoner)
  })
  
  output$t3_sankey_legend <- renderPlot({
    plot(
      NULL ,
      xaxt = 'n',
      yaxt = 'n',
      bty = 'n',
      ylab = '',
      xlab = '',
      xlim = 0:1,
      ylim = 0:1
    )
    legend(
      "topleft",
      legend = class2Color$class,
      pch = 15,
      pt.cex = 3,
      cex = 1.5,
      bty = 'n',
      col = class2Color$clolr
    )
    mtext("Classes", at = 0.2, cex = 2)
  })
}


shinyApp(ui, server)