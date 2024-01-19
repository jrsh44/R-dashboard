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
  tags$head(
    tags$title("League of Legends Stats"),
    tags$link(rel = "icon", href = "assets/favicon.png"),
    tags$link(rel = "stylesheet", href = "fonts.css"),
    tags$link(rel = "stylesheet", href = "styles.css"),
  ), 
  # Zakładka 1
  tabPanel("Ogólne",
    tags$div(class = "app-background",
      tags$div(class = "typoH1", "Ogólne statystyki"),
      tags$div(class = "typoH5", "Wybierz gracza i statystykę"),
      selectInput(inputId = "t1_player",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
      selectInput(inputId = "t1_stat",label = "Stat:", choices = c("kills", "deaths", "kda", "winrate", "gamesPlayed")),
      plotlyOutput("t1_champions_chart"),
      plotlyOutput("t1_time_played_chart")
    )
  ),
  
  # Zakładka 2
  tabPanel("Mapy",
    tags$div(class = "app-background",
      tags$div(class = "typoH1", "Przebieg meczów"),
      fluidRow(
        selectInput(inputId = "t2_map_player",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
        plotlyOutput("t2_heat_map"),
        plotlyOutput("t2_map"),
        plotlyOutput("t2_animated_map")
      ) 
    )
  ),
  
  # Zakładka 3
  tabPanel("Szczegółowe",
    tags$div(class = "app-background",
      tags$div(class = "typoH1", "Statystyki szczegółowe"),
      fluidRow(
        column(4,
          selectInput(inputId = "summoner",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
          selectInput(inputId = "Position",label = "Position",
                      choices = c("TOP","JUNGLE","MIDDLE","BOTTOM","UTILITY"),selected = "BOTTOM"),
          uiOutput("t3_dynamic_input"),
          uiOutput("t3_dynamic_input2"),
          selectInput(inputId = "type", label = "Type:",choices = c("Density","Chronologically"))),
        column(4,
          plotlyOutput("t3_dmg_per_death"),
          plotlyOutput("t3_minions_per_minute")
        ),
        column(4,
          plotlyOutput("t3_kill_participation"),
          plotlyOutput("t3_kda"))
      ),
      fluidRow(titlePanel("Itemki"),
        selectInput(inputId = "sankey_summoner",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
        sankeyNetworkOutput("t3_sankey"),
        plotOutput("t3_sankey_legend")
      )
    )
  )
)

server <- function(input, output) {

    # Zakładka 1
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
  
  output$t3_dynamic_input2 <- renderUI({
    if (input$type=="Density") {
      myStatsPosition <- df_item_champ %>% 
        filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
      if (nrow(myStatsPosition)==0) {
        selectInput(inputId = "compare",
                    label = "Compare with:",
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
  
  output$t3_dmg_per_death <- renderPlotly({
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath")
    }
  })
  
  output$t3_kill_participation <- renderPlotly({
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation")
    }
  })
  
  output$t3_minions_per_minute <- renderPlotly({
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute")
    }
  })
  
  output$t3_kda <- renderPlotly({
    if (input$type=='Density') {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda",input$compare)
    } else {
      f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda")
    }
  })

  output$t3_sankey <- renderSankeyNetwork({
    f_items_sankey_graph(input$sankey_summoner)
  })
  
  output$t3_sankey_legend <- renderPlot({
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", legend =class2Color$class, pch=15, pt.cex=3, cex=1.5, bty='n',
           col = class2Color$clolr)
    mtext("Classes", at=0.2, cex=2)
  })
}


shinyApp(ui, server)