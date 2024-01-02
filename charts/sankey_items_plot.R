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
  rename(name = champion_name, type = class, playStyle = playstyle) %>%
  mutate(
    name = case_when(
      name == "S<e9>raphine" ~ "Seraphine",
      name == "Wukong" ~ "MonkeyKing",
      name == "Miss Fortune" ~ "MissFortune",
      TRUE ~ name
    ),
    type = case_when(
      type == "Mage Artillery" ~ "Mage",
      type == "Controllerer" ~ "Controller",
      TRUE ~ type
    )
  )

new_champions <- data.frame(
  name = c('Nilah', 'Akshan', 'Vex', 'Zeri', 'RenataGlasc', 'Bel\'Veth', 'K\'Sante', 'Milio', 'Naafiri', 'Briar', 'Hwei'),
  type = c('Slayer', 'Slayer', 'Mage', 'Marksman', 'Controller', 'Slayer', 'Tank', 'Controller', 'Slayer', 'Fighter', 'Mage'),
  playStyle = c('Skirmisher', 'Assassin', 'Burst', 'Marksman', 'Enchanter', 'Skirmisher', 'Warden', 'Enchanter', 'Assassin', 'Diver', 'Artillery')
)

champion_stats <- bind_rows(champion_stats, new_champions)

class2Color <- data.frame(class = c("Fighter","Mage","Slayer","Tank","Marksman","Specialist", "Controller"),
                          clolr = c("#E18417","#7C17E1","#E41D1D","#00BF3B","#004AAD","#03F6FF","#EDCC23"))

legendary_items <- c(
  "Abyssal Mask",
  "Anathema's Chains",
  "Archangel's Staff",
  "Ardent Censer",
  "Atma's Reckoning",
  "Axiom Arc",
  "Banshee's Veil",
  "Black Cleaver",
  "Black Mist Scythe",
  "Blade of The Ruined King",
  "Bloodletter's Curse",
  "Bloodsong",
  "Bloodthirster",
  "Bulwark of the Mountain",
  "Chempunk Chainsword",
  "Chemtech Putrifier",
  "Cosmic Drive",
  "Dead Man's Plate",
  "Death's Dance",
  "Demonic Embrace",
  "Edge of Night",
  "Essence Reaver",
  "Fimbulwinter",
  "Force of Nature",
  "Frozen Heart",
  "Frozen Mallet",
  "Gargoyle Stoneplate",
  "Guardian Angel",
  "Hellfire Hatchet",
  "Hextech Gunblade",
  "Horizon Focus",
  "Hullbreaker",
  "Immortal Shieldbow",
  "Imperial Mandate",
  "Kaenic Rookern",
  "Knight's Vow",
  "Kraken Slayer",
  "Lich Bane",
  "Lightning Braid",
  "Lord Dominik's Regards",
  "Luden's Companion",
  "Malignance",
  "Manamune",
  "Maw of Malmortius",
  "Mejai's Soulstealer",
  "Mercurial Scimitar",
  "Mikael's Blessing",
  "Morellonomicon",
  "Mortal Reminder",
  "Muramana",
  "Nashor's Tooth",
  "Opportunity",
  "Pauldrons of Whiterock",
  "Perplexity", 
  "Phantom Dancer",
  "Profane Hydra",
  "Prowler's Claw",
  "Rabadon's Deathcap",
  "Randuin's Omen",
  "Rapid Firecannon",
  "Ravenous Hydra",
  "Rectrix",
  "Redemption",
  "Rite of Ruin",
  "Runaan's Hurricane",
  "Runic Compass",
  "Rylai's Crystal Scepter",
  "Seraph's Embrace",
  "Serpent's Fang",
  "Serylda's Grudge",
  "Shadowflame",
  "Shard of True Ice",
  "Shattered Armguard",
  "Silvermere Dawn",
  "Sin Eater",
  "Solstice Sleigh",
  "Spear of Shojin",
  "Spectral Cutlass",
  "Spirit Visage",
  "Staff of Flowing Water",
  "Statikk Shiv",
  "Steel Sigil", 
  "Sterak's Gage",
  "Stormrazor",
  "Stormsurge",
  "Sundered Sky",
  "Sunfire Aegis",
  "Sword of the Blossoming Dawn",
  "Terminus",
  "The Collector",
  "Thornmail",
  "Titanic Hydra",
  "Trailblazer",
  "Tunneler",
  "Turbo Chemtank",
  "Umbral Glaive",
  "Void Staff" ,
  "Voltaic Cyclosword",
  "Warmog's Armor",
  "Winter's Approach",
  "Wit's End",
  "Wordless Promise",
  "Zeke's Convergence",
  "Zhonya's Hourglass",
  "Crown of the Shattered Queen",
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
  "Youmuu's Ghostblade"
)

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
  mythicItemName = mythicitems,
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
itemsNormal <- data.frame(itemId = numeric(),
                          itemName = character(),
                          depth = numeric()
)

for (item in items$data) {
  if (item$maps$'11') {
    itemsNormal <- rbind(itemsNormal, data.frame(itemId = as.numeric(gsub('.png',"",item$image$full)),
                                                 itemName = item$name,
                                                 depth = ifelse(is.null(item$depth),1,item$depth)))
  }
}

itemsNormal <- itemsNormal %>% mutate(depth = ifelse(itemId %in% as.numeric(unlist(items$data$'1001'$into)),3,depth))

#LISTA LEGEND I MITHIC NA NORMALU
mythicitemsId <- left_join(itemsNormal %>% filter(itemName %in% mythicitems) %>% select(itemId,itemName),mythicitemsDf,join_by('itemName'=='mythicItemName'))
legendaryitemsNormal <- itemsNormal %>% filter(tolower(itemName) %in% tolower(legendary_items)) %>% mutate(mythic = ifelse(itemName %in% mythicitems,1,0)) 
boots <- c(3006,3009,3020,3047,3111,3117,3158)

plotChampClassItem  <-  function(){

  df_csv <- read.csv("./db/playerMatchStats.csv")

  myStats <- df_csv %>% filter(!is.na(mythic_item)) %>% select(champion_name,mythic_item)
  GroupChampItemTemp <- left_join(myStats,champion_stats %>% select(name,type),join_by('champion_name'=='name'))
  GroupChampItem <- left_join(GroupChampItemTemp,mythicitemsId %>% select(itemId,itemName),join_by('mythic_item'=='itemId'))
  
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
  ),champion_stats %>% select(name,type),join_by('name'=='name')) %>%
    mutate(type = ifelse(is.na(type) & name %in% unique(champion_stats$type),
                         name,type)) 
  nodes <- left_join(nodesTemp,mythicitemsId %>% select(itemName,recomanded),join_by('name'=='itemName')) %>%
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
  return(p)
}

server2 <- function(input,output){
  output$plot1 <- renderSankeyNetwork({
    plotChampClassItem()
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
