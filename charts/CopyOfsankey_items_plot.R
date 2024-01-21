library(httr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(networkD3)

#ITEMY
url_items <- "https://ddragon.leagueoflegends.com/cdn/13.24.1/data/en_US/item.json?fbclid=IwAR2_R3caRPfpTpAd8-5RpbvDjcGW9CJLIfP5yjsQ2xN_B9uS5ay2HYB2U4Q"
items <- content(GET(url_items), "parsed")

#TYPY CHAMPIONÓW
champion_stats <- read.csv("./db/championsStats.csv", sep = ';')
champion_stats <- champion_stats %>%
  select(champion_name, class, playstyle) %>%
  rename(name = champion_name, type = class, playstyle = playstyle) %>%
  mutate(
    name = case_when(
      name == "S<e9>raphine" ~ "Seraphine",
      name == "Wukong" ~ "MonkeyKing",
      TRUE ~ name
    ),
    type = case_when(
      type == "Mage Artillery" ~ "Mage",
      type == "Controllerer" ~ "Controller",
      TRUE ~ type
    )
  ) %>% mutate(name = str_replace(name,' ',''))

new_champions <- data.frame(
  name = c('Nilah', 'Akshan', 'Vex', 'Zeri', 'RenataGlasc', 'Bel\'Veth', 'K\'Sante', 'Milio', 'Naafiri', 'Briar', 'Hwei'),
  type = c('Slayer', 'Slayer', 'Mage', 'Marksman', 'Controller', 'Slayer', 'Tank', 'Controller', 'Slayer', 'Fighter', 'Mage'),
  playstyle = c('Skirmisher', 'Assassin', 'Burst', 'Marksman', 'Enchanter', 'Skirmisher', 'Warden', 'Enchanter', 'Assassin', 'Diver', 'Artillery')
)

champion_stats <- bind_rows(champion_stats, new_champions)

class2Color <- data.frame(class = c("Fighter","Mage","Slayer","Tank","Marksman","Specialist", "Controller"),
                          clolr = c("#E18417","#7C17E1","#E41D1D","#00BF3B","#004AAD","#03F6FF","#EDCC23"))

mythicitems <- c( "Crown of the Shattered Queen",
                  "Divine Sunderer",
                  "Duskblade of Draktharr",
                  "Echoes of Helia",
                  "Eclipse",
                  "Evenshroud",
                  "Everfrost",
                  "Galeforce",
                  "Goredrinker",
                  "Guinsoo's Rageblade",
                  "Heartsteel",
                  "Hextech Rocketbelt",
                  "Iceborn Gauntlet",
                  "Infinity Edge",
                  "Jak'Sho, The Protean",
                  "Liandry's Anguish",
                  "Locket of the Iron Solari",
                  "Luden's Tempest",
                  "Moonstone Renewer",
                  "Navori Quickblades",
                  "Night Harvester",
                  "Radiant Virtue",
                  "Riftmaker",
                  "Rod of Ages",
                  "Shurelya's Battlesong",
                  "Stridebreaker",
                  "Trinity Force",
                  "Youmuu's Ghostblade")

mythicitemsDf <- data.frame(
  mythic_Item_Name = mythicitems,
  recomanded = c("Mage",
                 "Fighter",
                 "Slayer",
                 "Controller",
                 "Slayer",
                 "Controller",
                 "Mage",
                 "Marksman",
                 "Fighter",
                 "Marksman",
                 "Tank",
                 "Mage",
                 "Tank",
                 "Marksman",
                 "Tank",
                 "Mage",
                 "Controller",
                 "Mage",
                 "Controller",
                 "Marksman",
                 "Mage",
                 "Controller",
                 "Mage",
                 "Mage",
                 "Controller",
                 "Fighter",
                 "Fighter",
                 "Slayer")
)

#Lista Itemów na NORMALU
itemsNormal <- data.frame(item_Id = numeric(),
                          item_Name = character()
)

for (item in items$data) {
  if (item$maps$'11') {
    itemsNormal <- rbind(itemsNormal, data.frame(item_Id = as.numeric(gsub('.png',"",item$image$full)),
                                                 item_Name = item$name))
  }
}

#LISTA LEGEND I MITHIC NA NORMALU
mythicItemsId <- left_join(itemsNormal %>% filter(item_Name %in% mythicitems) %>% select(item_Id,item_Name),mythicitemsDf,join_by('item_Name'=='mythic_Item_Name'))
#boots <- c(3006,3009,3020,3047,3111,3117,3158)


summoner <- data.frame(name = c("Cwalina","Borycki","Jarosz"),
                       puuid = c( "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA",
                                  "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA",
                                  "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"))

plotChampClassItem  <-  function(player){

  df_csv <- read.csv("./db/playerMatchStats.csv") %>% filter(player_id == as.vector(summoner %>% filter(name %in% player) %>% select(puuid)))

  myStats <- df_csv %>% filter(!is.na(mythic_item)) %>% select(champion_name,mythic_item)
  GroupChampItemTemp <- left_join(myStats,champion_stats %>% select(name,type),join_by('champion_name'=='name'))
  GroupChampItem <- left_join(GroupChampItemTemp,mythicItemsId %>% select(item_Id,item_Name),join_by('mythic_item'=='item_Id'))
  
  champ_type_link <- GroupChampItem %>% select(champion_name,type) %>% group_by(champion_name,type) %>% count()
  champ_item_link <-  GroupChampItem %>% select(champion_name,item_Name) %>% group_by(champion_name,item_Name) %>% count()
  
  links <- data.frame(
    source=c(champ_type_link$type,champ_item_link$champion_name), 
    target=c(champ_type_link$champion_name,champ_item_link$item_Name), 
    value=c(champ_type_link$n,champ_item_link$n)
  )
  
  nodesTemp <- left_join(data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  ),champion_stats %>% select(name,type),join_by('name'=='name')) %>%
    mutate(type = ifelse(is.na(type) & name %in% unique(champion_stats$type),
                         name,type)) 
  nodes <- left_join(nodesTemp,mythicItemsId %>% select(item_Name,recomanded),join_by('name'=='item_Name')) %>%
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
                     sinksRight=FALSE, NodeGroup = "type",
                     colourScale = JS(tekst2))


  return(p)
}

server2 <- function(input,output){
  output$plot1 <- renderSankeyNetwork({
    plotChampClassItem(input$summoner)
  })
  
  output$legend <- renderPlot({
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", legend =class2Color$class, pch=15, pt.cex=3, cex=1.5, bty='n',
           col = class2Color$clolr)
    mtext("Classes", at=0.2, cex=2)
  })
}

ui2 <- fluidPage(titlePanel("Title"),
                 selectInput(inputId = "summoner",label = "Summoner:", choices = c("Cwalina","Borycki","Jarosz")),
                 sankeyNetworkOutput("plot1"),
                 plotOutput("legend")
)

runApp(shinyApp(ui2,server2))
