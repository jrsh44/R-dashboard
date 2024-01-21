library(jpeg)
library(png)
library(base64enc)
library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
library(httr)
library(networkD3)
library(shinythemes)

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
  title = "League of Legend - stats",

  # Zakładka 1
  tabPanel("Ogólne",
      tags$head(
        tags$title("League of Legends Stats"),
        tags$link(rel = "icon", href = "assets/favicon.png"),
        tags$link(rel = "stylesheet", href = "fonts.css"),
        tags$link(rel = "stylesheet", href = "styles.css"),
      ),
      tags$div(class = "tab1-wrapper",
        tags$div(class = "tab1-title-text",
          tags$div(class = "typoH1", "Ogólne statystyki"),
          tags$img(class = "tab1-decorator-lg", src = "assets/decorator-hr-lg.png")
        ),
        tags$div(class = "tab1-champ-container", 
          tags$div(class = "tab1-champ-wrapper",
            tags$div(class = "tab1-title-text",
              tags$div(class = "typoH2", "Porównanie championów"),
              tags$div(class = "typoH5", "którymi gramy"),
              tags$img(class = "tab1-decorator", src = "assets/decorator-hr.png"),
            ),
            tags$div(class = "typoBodyBold", "Obecnie w grze dostępnych jest ponad 150 postaci podzielonych na różne kategorie, takie jak zabójcy, strażnicy, magowie czy strzelcy. Stojąc przed takim szerokim wyborem istotne staje się określenie, którym bohaterem radzimy sobie najlepiej,"),
            #tags$div(class = "typoBody", textOutput("t1_stat_desc")),
            tags$div(class = "tab1-champ-buttons", 
              selectInput(inputId = "t1_player",label = "Gracz:", choices = c("Cwalina","Borycki","Jarosz")),
              selectInput(inputId = "t1_stat",label = "Statystyka:", choices = c("Zabójstwa", "Śmierci", "Współczynnik KDA", "Współczynnik zwycięstw", "Liczba gier")),
            ),
          ),
          tags$div(class = "tab1-champ-wrapper",
            plotlyOutput("t1_champions_chart"),
          ),
        )
      # plotlyOutput("t1_time_played_chart")
     )
  ),
  
  # Zakładka 2
  tabPanel("Mapy",
    tags$div(class = "typoH1", "Przebieg meczów"),
    fluidRow(
      selectInput(inputId = "t2_map_player",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
      plotlyOutput("t2_heat_map"),
      plotlyOutput("t2_map"),
      plotlyOutput("t2_animated_map")
    )
  ),
  
  # Zakładka 3
  tabPanel("Szczegółowe",
           tags$div(class = "tab1-wrapper",
                    tags$div(class = "tab1-title-text",
                             tags$div(class = "typoH1", "Szczegółowe statystyki"),
                             tags$img(class = "tab1-decorator-lg", src = "assets/decorator-hr-lg.png")
                    )),
    fluidRow(
      tags$div(class = "typoH2", "Statystyki Przywoływacza"),
      column(4,
        selectInput(inputId = "summoner",label = "Summoner", choices = c("Cwalina","Borycki","Jarosz")),
        uiOutput("t3_dynamic_input_0"),
        uiOutput("t3_dynamic_input"),
        uiOutput("t3_dynamic_input2"),
        selectInput(inputId = "type", label = "Type",choices = c("Density","Chronologically")),
        tags$div(class = "typoBodyBold", "W League of Legends nie istnieje skala, która jednoznacznie potrafiłaby określić poziom umiejętności przywoływacza. Ze względu na możliwość wyboru jednej z pięciu ról (pozycji) oraz jednego z około 150 championów określenie wpływu na przebieg gry jest co najmniej bardzo trudnym zadaniem. Wraz z zespołem ustaliliśmy jednak 4 statystyki mogące stanowić o poziomie gracza.")),
      column(4,
        plotlyOutput("t3_dmg_per_death"),
        plotlyOutput("t3_minions_per_minute")
      ),
      column(4,
        plotlyOutput("t3_kill_participation"),
        plotlyOutput("t3_kda"))
    ),
    fluidRow(
             tags$div(class = "typoH2", "Wybór championów i Itemów"),
      selectInput(inputId = "sankey_summoner",label = "Summoner", choices = c("Cwalina","Borycki","Jarosz")),
      tags$div(class = "typoBodyBold", "Poprzez zabójstwa, wykonywanie zadań, niszczenie bódowli przeciwnika czy 'farmienie' minionów gracz otrzymuje złoto, które następnie może być wymienione w sklepie na przedmioty. Najcenniejsze, oznaczone tagiem 'Mythic' dają unikalne wzmocnienia. Z tego względu każdy przywoływacz może używać tylko 1 przedmiotu mitycznego w danym momencie. Ze względu na umiejętności postaci i ich użyteczność w różnych aspektach rozgrywki, pewne przedmioty są szczególnie sugerowane pewnym klasom championów, co obrazuje poniższy wykres."),
      #sankeyNetworkOutput("t3_sankey_legend"),
      sankeyNetworkOutput("t3_sankey")
    )
  ),

  # ABOUT PAGE
  tabPanel("About",
    tags$div(class = "about-container", 
    tags$div(class = "typoH5", "League of Legends - Analise"),
    tags$div(class = "typoH1", "Informacje o projekcie"),
    tags$img(class = "tab4-decorator-lg", src = "assets/decorator-hr-lg.png"),
    )
  ),
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
      f_plot_time(input$t1_player)
    })
    

    # Zakładka 2

    output$t2_animated_map <- renderPlotly({
      f_animated_map(input$t2_map_player,c("death","kill"),c("Orianna"),c("MIDDLE"),win=c(T,F),team_id = c(200,100))
    })

    output$t2_heat_map <- renderPlotly({
      f_heat_map(input$t2_map_player,c("kill","death"),c("Orianna"),c("MIDDLE"),win=c(T,F),team_id = c(200,100))
    })

    output$t2_map <- renderPlotly({
      f_map(input$t2_map_player,c("death","kill","assist"),c("Shaco"),c("JUNGLE"),win=c(T,F),team_id = c(200,100))
    })



    # Zakładka 3
    # output$t3_dynamic_input_0 <- renderUI({
    #   
    #   positions <- c("TOP","JUNGLE","MIDDLE","BOTTOM","UTILITY")
    #   myStatsPosition <-  unique((df_item_champ %>% dplyr::filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner))))$position)
    #   selectInput(inputId = "Position",label = "Position",
    #               choices = positions[positions %in% myStatsPosition])
    #   
    # })
    
    observeEvent(input$summoner, {
      output$t3_dynamic_input_0 <- renderUI({
        positions <- c("TOP","JUNGLE","MIDDLE","BOTTOM","UTILITY")
        myStatsPosition <-  unique((df_item_champ %>% dplyr::filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner))))$position)
        selectInput(inputId = "Position",label = "Position",
                    choices = positions[positions %in% myStatsPosition])
        
      })
    })
    
  #   output$t3_dynamic_input <- renderUI({
  #     
  #       myStatsPosition <- df_item_champ %>% 
  #         dplyr::filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
  #       if (nrow(myStatsPosition)==0) {
  #         selectInput(inputId = "id1",
  #                     label = "Champion",
  #                     choices = c("None"))
  #       } else {
  #         selectInput(inputId = "id1",label = "Champion", choices = c("All",unique(unlist(myStatsPosition$champion_name))))
  #       }
  #     
  # })
    
    observeEvent(input$Position, {
      output$t3_dynamic_input <- renderUI({
        myStatsPosition <- df_item_champ %>% 
          dplyr::filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
        if (nrow(myStatsPosition)==0) {
          selectInput(inputId = "id1",
                      label = "Champion",
                      choices = c("None"))
        } else {
          selectInput(inputId = "id1",label = "Champion", choices = c("All",unique(unlist(myStatsPosition$champion_name))))
        }
        
      })
    })
  
    
    observeEvent(input$id1, {
      output$t3_dynamic_input2 <- renderUI({
        if (input$type=="Density") {
          myStatsPosition <- df_item_champ %>% 
            filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
          if (nrow(myStatsPosition)==0) {
            selectInput(inputId = "compare",
                        label = "Compare with",
                        choices = c("None"))
          } else {
            champs <- unique(unlist(myStatsPosition$champion_name))
            if (input$id1=="All" | input$id1=="None") {
              selectInput(inputId = "compare", label = "Compare with:", choices = c("Don't"))
            } else {
              selectInput(inputId = "compare", label = "Compare with:", choices = c("Don't","All",champs[champs != input$id1]))
            }
          }
        }
      })
    })
  # output$t3_dynamic_input2 <- renderUI({
  #   if (input$type=="Density") {
  #     myStatsPosition <- df_item_champ %>% 
  #       filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
  #     if (nrow(myStatsPosition)==0) {
  #       selectInput(inputId = "compare",
  #                   label = "Compare with:",
  #                   choices = c("None"))
  #     } else {
  #       champs <- unique(unlist(myStatsPosition$champion_name))
  #       if (input$id1=="All" | input$id1=="None") {
  #         selectInput(inputId = "compare", label = "Compare with:", choices = c("Don't"))
  #       } else {
  #         selectInput(inputId = "compare", label = "Compare with:", choices = c("Don't","All",champs[champs != input$id1]))
  #       }
  #     }
  #   }
  # })

      output$t3_dmg_per_death <- renderPlotly({
        if (!is.null(input$id1)){
        if (input$type=='Density') {
          f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath",input$compare)
        } else {
          f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath")
        }
        }
      })

    
  output$t3_dmg_per_death <- renderPlotly({
    if (!is.null(input$id1)){
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath")
    }
    }
  })
  
  output$t3_kill_participation <- renderPlotly({
    if (!is.null(input$id1)){
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation")
    }
    }
  })
  
  output$t3_minions_per_minute <- renderPlotly({
    if (!is.null(input$id1)){
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute")
    }
    }
  })
  
  output$t3_kda <- renderPlotly({
    if (!is.null(input$id1)){
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda")
    }
    }
  })

  output$t3_sankey <- renderSankeyNetwork({
   #  
   #  JS <-
   #    '
   #  function(el, x, data){
   #    var svg = d3.select("t3-sankey-wrapper")
   #    // Handmade legend
   #    svg.append("circle").attr("cx",25).attr("cy",10).attr("r", 4).style("fill", "#E18417")
   #    svg.append("circle").attr("cx",90).attr("cy",10).attr("r", 4).style("fill", "#7C17E1")
   #    svg.append("circle").attr("cx",150).attr("cy",10).attr("r", 4).style("fill", "#E41D1D")
   #    svg.append("circle").attr("cx",215).attr("cy",10).attr("r", 4).style("fill", "#00BF3B")
   #    svg.append("circle").attr("cx",270).attr("cy",10).attr("r", 4).style("fill", "#004AAD")
   #    svg.append("circle").attr("cx",360).attr("cy",10).attr("r", 4).style("fill", "#03F6FF")
   #    svg.append("circle").attr("cx",445).attr("cy",10).attr("r", 4).style("fill", "#EDCC23")
   # 
   #    svg.append("text").attr("x", 35).attr("y", 10).text("Fighter").style("font-size", "13px").attr("alignment-baseline","middle")
   #    svg.append("text").attr("x", 100).attr("y", 10).text("Mage").style("font-size", "13px").attr("alignment-baseline","middle")
   # svg.append("text").attr("x", 160).attr("y", 10).text("Slayer").style("font-size", "13px").attr("alignment-baseline","middle")
   # svg.append("text").attr("x", 225).attr("y", 10).text("Tank").style("font-size", "13px").attr("alignment-baseline","middle")
   # svg.append("text").attr("x", 280).attr("y", 10).text("Marksman").style("font-size", "13px").attr("alignment-baseline","middle")
   # svg.append("text").attr("x", 370).attr("y", 10).text("Specialist").style("font-size", "13px").attr("alignment-baseline","middle")
   # svg.append("text").attr("x", 455).attr("y", 10).text("Controller").style("font-size", "13px").attr("alignment-baseline","middle")
   #  }
   #  '


    htmlwidgets::onRender(f_items_sankey_graph(input$sankey_summoner),
                                             'function(el, x) {
    d3.selectAll(".node text")
        .style("fill", "#c8aa6e").style("font-size","14px");
  }'
    )
    
  })
  
  output$t3_sankey_legend <- renderSankeyNetwork({
    htmlwidgets::onRender(f_sankey_lenend_graph(),
                          'function(el, x) {
    d3.selectAll(".node text")
        .style("fill", "#c8aa6e").style("font-size","14px");
  }'
    )
  })
  
}


shinyApp(ui, server)