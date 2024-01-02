library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)

ItemChampGroup <- read.csv("./db/playerMatchStats.csv")

server <- function(input, output) {
  
  univPlot <- function(df_csv,type,stat){
    myStatsPosition2 <- myStatsReactive()
    if (nrow(myStatsPosition2)!=0) {
      if (type=="Density") {
        if (stat!="kda") {
          gg <- ggplot(data = myStatsPosition2,aes_string(x = stat)) +
            geom_density(aes(fill = compare),alpha = 0.5) + 
            labs() 
          
        }else{
          gg <- ggplot(data = myStatsPosition2 %>% mutate(deaths = ifelse(deaths==0,1,deaths)),aes(x = (kills+assists)/deaths)) +
            geom_density(aes(fill = compare),alpha = 0.5) + 
            labs() 
        }
        gg <- ggplotly(gg)
      } else if (type=="Time") {
        myStatsPosition3 <- myStatsReactive2()
        
        if(stat=="kills"){
          gg <- plot_ly(data = myStatsPosition3,
                        x = ~n,
                        y = ~kills,
                        type = 'scatter',
                        mode = 'lines+markers')
        } else if (stat=="deaths") {
          gg <- plot_ly(data = myStatsPosition3,
                        x = ~n,
                        y = ~deaths,
                        type = 'scatter',
                        mode = 'lines+markers')
        } else if (stat=="assists"){
          gg <- plot_ly(data = myStatsPosition3,
                        x = ~n,
                        y = ~assists,
                        type = 'scatter',
                        mode = 'lines+markers')
        } else {
          gg <- plot_ly(data = myStatsPosition3 %>% mutate(deaths = ifelse(deaths==0,1,deaths)),
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
  
  output$dynamicInput <- renderUI({
    myStatsPosition <- ItemChampGroup %>% filter(position==input$Position)
    if (nrow(myStatsPosition)==0) {
      selectInput(inputId = "id1",
                  label = "Champion",
                  choices = c("None"))
    } else {
      selectInput(inputId = "id1",label = "Champion", choices = c("All",unique(unlist(myStatsPosition$champion_name))))
    }
  })
  
  output$dynamicInput2 <- renderUI({
    if (input$type=="Density") {
      myStatsPosition <- ItemChampGroup %>% filter(position==input$Position)
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
  
  myStatsReactive <- reactive({
    if (input$id1=="All") {
      myStatsPosition2 <- ItemChampGroup %>%
        filter(position==input$Position) %>% mutate(compare = "All") 
    } else if (input$id1=="None"){
      myStatsPosition2 <- data.frame() %>% mutate(compare = "None") 
    } else {
      if (input$type=="Density") {
        myStatsPosition2 <- ItemChampGroup %>%
          filter(position==input$Position) %>% mutate(compare = ifelse(champion_name==input$id1,input$id1,input$compare))
        if (input$compare=="Don't") {
          myStatsPosition2 <- myStatsPosition2 %>% filter(champion_name==input$id1)
        } else if (input$compare!="All") {
          myStatsPosition2 <- myStatsPosition2 %>% filter(champion_name %in% c(input$id1,input$compare))
        }
      } else {
        myStatsPosition2 <- ItemChampGroup %>% filter(position==input$Position,champion_name==input$id1)
      } 
    }
    return(myStatsPosition2)
  })
  
  myStatsReactive2 <- reactive({
    myStatsTemp <- myStatsReactive()
    return(myStatsTemp %>% arrange(match_start_time) %>% mutate(n = 1:nrow(myStatsTemp)))
  })
  
  output$Kills <- renderPlotly({
    univPlot(ItemChampGroup,input$type,"kills")
  })
  
  output$Deaths <- renderPlotly({
    univPlot(ItemChampGroup,input$type,"deaths")
  })
  
  output$Assists <- renderPlotly({
    univPlot(ItemChampGroup,input$type,"assists")
  })
  
  output$Kda <- renderPlotly({
    univPlot(ItemChampGroup,input$type,"kda")
  })
}

ui <- fluidPage(titlePanel("Title"),
                
                fluidRow(
                  column(4,
                         selectInput(inputId = "Position",label = "Position",
                                     choices = c("TOP","JUNGLE","MIDDLE","BOTTOM","UTILITY"),selected = "BOTTOM"),
                         uiOutput("dynamicInput"),
                         uiOutput("dynamicInput2"),
                         selectInput(inputId = "type", label = "Type:",choices = c("Density","Time"))),
                  column(4,
                         plotlyOutput("Kills"),
                         plotlyOutput("Assists")
                  ),
                  column(4,
                         plotlyOutput("Deaths"),
                         plotlyOutput("Kda")))
                
)

runApp(shinyApp(ui,server))


plotChampClassItem  <-  function(df_csv){
  myStats <- df_csv %>% filter(!is.na(itemMythic)) %>% select(champion_name,itemMythic)
  GroupChampItemTemp <- left_join(myStats,ChampionStats %>% select(name,type),join_by('champion_name'=='name'))
  GroupChampItem <- left_join(GroupChampItemTemp,mythicItemsId %>% select(itemId,itemName),join_by('itemMythic'=='itemId'))
  
  df21 <- GroupChampItem %>% select(champion_name,type) %>% group_by(champion_name,type) %>% count()
  df22 <-  GroupChampItem %>% select(champion_name,itemName) %>% group_by(champion_name,itemName) %>% count()
  
  links <- data.frame(
    source=c(df21$type,df22$champion_name), 
    target=c(df21$champion_name,df22$itemName), 
    value=c(df21$n,df22$n)
  )
  
  nodesTemp <- left_join(data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  ),ChampionStats %>% select(name,type),join_by('name'=='name')) %>%
    mutate(type = ifelse(is.na(type) & name %in% unique(ChampionStats$type),
                         name,type)) 
  nodes <- left_join(nodesTemp,mythicItemsId %>% select(itemName,recomanded),join_by('name'=='itemName')) %>%
    mutate(type = ifelse(is.na(type),recomanded,type))
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  #kolejność kolorów ,posortować
  tekst0 <- left_join(data.frame(type = unique(nodes$type)),class2Color,join_by('type'=='class'))$clolr
  tekst1 <- paste(c("'",paste(tekst0,collapse = "', '"),"'"),collapse=  "")
  tekst2 <- paste(c('d3.scaleOrdinal( [',tekst1,']);'),collapse = "")
  
  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE,NodeGroup = "type",
                     colourScale = JS(tekst2))
  p 
}

server2 <- function(input,output){
  output$plot1 <- renderSankeyNetwork({
    plotChampClassItem(ItemChampGroup)
  })
  
  output$legend <- renderPlot({
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", legend =class2Color$class, pch=15, pt.cex=3, cex=1.5, bty='n',
           col = class2Color$clolr)
    mtext("Classes", at=0.2, cex=2)
  })
}

ui2 <- fluidPage(titlePanel("Title"),
                 sankeyNetworkOutput("plot1"),
                 plotOutput("legend")
)

runApp(shinyApp(ui2,server2))
