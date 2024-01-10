# # For testing purposes only
# library(ggplot2)
# library(tidyverse)
# library(shiny)
# library(plotly)
# puuid_cwalina <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_borycki <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_jarosz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

#do POPRAWY
filterForMatches <- read.csv("./db/playerMatchStats.csv") %>% group_by(player_id,position,champion_name) %>% count() %>% filter(n > 1) %>% select(player_id,champion_name)
df_item_champ <- inner_join(read.csv("./db/playerMatchStats.csv"),filterForMatches,by = c('player_id','position','champion_name'))

summoner <- data.frame(name = c("Cwalina","Borycki","Jarosz"),
                       puuid = c( "uUj9Y1pGLa9_zICoJji5ea3m5hKsDrj1zdKY2GmtdROp8DY5y__gcNo-2c6k-AhJbtQhUQs2gtviaQ",
                                  "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA",
                                  "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"))

f_stats_position_reactive <- function(summoner_name,summoner_position,id1,type,compare = "brak"){
  myStatsPosition1 <- df_item_champ %>% filter(player_id == as.vector(summoner %>% filter(name %in% summoner_name) %>% select(puuid)),position==summoner_position)
  if (type=="Density") {
    if (id1=="All") {
      myStatsPosition2 <- myStatsPosition1 %>%
        filter(position==summoner_position) %>% mutate(compare = "All") 
    } else if (id1=="None"){
      myStatsPosition2 <- data.frame() %>% mutate(compare = "None") 
    } else {
        myStatsPosition2 <- df_item_champ %>%
          filter(player_id == as.vector(summoner %>% filter(name %in% summoner_name) %>% select(puuid)),position==summoner_position) %>% mutate(compare = ifelse(champion_name==id1,id1,compare))
        if (compare=="Don't") {
          myStatsPosition2 <- myStatsPosition2 %>% filter(champion_name==id1)
        } else if (compare!="All") {
          myStatsPosition2 <- myStatsPosition2 %>% filter(champion_name %in% c(id1,compare))
        }
    }
  } else {
      if (id1=='All'){
        myStatsPosition2temp <- df_item_champ %>% filter(player_id == as.vector(summoner %>% filter(name %in% summoner_name) %>% select(puuid)),position==summoner_position)
      } else {
        myStatsPosition2temp <- df_item_champ %>% filter(player_id == as.vector(summoner %>% filter(name %in% summoner_name) %>% select(puuid)),position==summoner_position,champion_name==id1)
      }
      View(myStatsPosition2temp)
      myStatsPosition2 <- myStatsPosition2temp %>% arrange(match_start_time) %>% mutate(n = 1:nrow(myStatsPosition2temp))
    } 
  return(myStatsPosition2)
}
  
f_overall_stats_plot <- function(summoner_name,summoner_position,id1,type,stat,compare = "brak"){
  myStatsPosition2 <- f_stats_position_reactive(summoner_name,summoner_position,id1,type,compare)
  if (nrow(myStatsPosition2)!=0) {
    if (type=="Density") {
      if (stat=="DmgPerDeath") {
        gg <- ggplot(data = myStatsPosition2 %>% mutate(deaths = ifelse(deaths==0,1,deaths)),aes(x = total_damage_dealt_to_champions/deaths)) +
          geom_density(aes(fill = compare),alpha = 0.5) + 
          labs() 
        
      } else if (stat=="kill_participation") {
        gg <- ggplot(data = myStatsPosition2,aes(x = kill_participation)) +
          geom_density(aes(fill = compare),alpha = 0.5) + 
          labs() 
        
      } else  if (stat=="MinionsPerMinute") {
        gg <- ggplot(data = myStatsPosition2,aes(x = total_minions_killed/(game_length/60))) +
          geom_density(aes(fill = compare),alpha = 0.5) + 
          labs() 
        
      } else {
        gg <- ggplot(data = myStatsPosition2 %>% mutate(deaths = ifelse(deaths==0,1,deaths)),aes(x = (kills+assists)/deaths)) +
          geom_density(aes(fill = compare),alpha = 0.5) + 
          labs() 
      }
      gg <- ggplotly(gg)
    } else if (type=="Chronologically") {
      if(stat=="DmgPerDeath"){
        gg <- plot_ly(data = myStatsPosition2,
                      x = ~n,
                      y = ~total_damage_dealt_to_champions/deaths,
                      type = 'scatter',
                      mode = 'lines+markers')
      } else if (stat=="kill_participation") {
        gg <- plot_ly(data = myStatsPosition2,
                      x = ~n,
                      y = ~kill_participation,
                      type = 'scatter',
                      mode = 'lines+markers')
      } else if (stat=="MinionsPerMinute"){
        gg <- plot_ly(data = myStatsPosition2,
                      x = ~n,
                      y = ~total_minions_killed/(game_length/60),
                      type = 'scatter',
                      mode = 'lines+markers')
      } else {
        gg <- plot_ly(data = myStatsPosition2 %>% mutate(deaths = ifelse(deaths==0,1,deaths)),
                      x = ~n,
                      y = ~(kills + assists)/deaths,
                      type = 'scatter',
                      mode = 'lines+markers')
      }
    }
  } else {
    gg <- ggplot(data = data.frame(brak = "brak")) + geom_text(aes(x = 1,y = 1,label = brak))
    gg <- ggplotly(gg)
  }
  return(gg)
}



# server <- function(input, output) {
#   # POPRAWIĆ FILTROWANIE PO GRACZU W dynamicinput,dynamicinput2, myStatsReactive
#   output$dynamicInput <- renderUI({
#     myStatsPosition <- df_item_champ %>% filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
#     if (nrow(myStatsPosition)==0) {
#       selectInput(inputId = "id1",
#                   label = "Champion",
#                   choices = c("None"))
#     } else {
#       selectInput(inputId = "id1",label = "Champion", choices = c("All",unique(unlist(myStatsPosition$champion_name))))
#     }
#   })
  
#   output$dynamicInput2 <- renderUI({
#     if (input$type=="Density") {
#       myStatsPosition <- df_item_champ %>% filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
#       if (nrow(myStatsPosition)==0) {
#         selectInput(inputId = "compare",
#                     label = "Compare with:",
#                     choices = c("None"))
#       } else {
#         champs <- unique(unlist(myStatsPosition$champion_name))
#         if (input$id1=="All" | input$id1=="None") {
#           selectInput(inputId = "compare", label = "Compare with:", choices = c("Don't"))
#         } else {
#           selectInput(inputId = "compare", label = "Compare with:", choices = c("Don't","All",champs[champs != input$id1]))
#         }
#       }
#     }
#   })
  

#   output$DmgPerDeath <- renderPlotly({
#     if (input$type=='Density') {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath",input$compare)
#     } else {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath")
#     }
#   })
  
#   output$kill_participation <- renderPlotly({
#     if (input$type=='Density') {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation",input$compare)
#     } else {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation")
#     }
#   })
  
#   output$MinionsPerMinute <- renderPlotly({
#     if (input$type=='Density') {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute",input$compare)
#     } else {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute")
#     }
#   })
  
#   output$Kda <- renderPlotly({
#     if (input$type=='Density') {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda",input$compare)
#     } else {
#       f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda")
#     }
#   })
# }

# ui <- fluidPage(titlePanel("Title"),
                
#                 fluidRow(
#                   column(4,
#                          selectInput(inputId = "summoner",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
#                          selectInput(inputId = "Position",label = "Position",
#                                      choices = c("TOP","JUNGLE","MIDDLE","BOTTOM","UTILITY"),selected = "BOTTOM"),
#                          uiOutput("dynamicInput"),
#                          uiOutput("dynamicInput2"),
#                          selectInput(inputId = "type", label = "Type:",choices = c("Density","Chronologically"))),
#                   column(4,
#                          plotlyOutput("DmgPerDeath"),
#                          plotlyOutput("MinionsPerMinute")
#                   ),
#                   column(4,
#                          plotlyOutput("kill_participation"),
#                          plotlyOutput("Kda")))
                
# )

# runApp(shinyApp(ui,server))