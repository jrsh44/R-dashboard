library(httr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(jpeg)
library(tidyverse)
library(shiny)
library(plotly)
library(patchwork)
library(networkD3)

#ITEMY
urlItems <- "https://ddragon.leagueoflegends.com/cdn/13.24.1/data/en_US/item.json?fbclid=IwAR2_R3caRPfpTpAd8-5RpbvDjcGW9CJLIfP5yjsQ2xN_B9uS5ay2HYB2U4Q"
Items <- content(GET(urlItems), "parsed")

#TYPY CHAMPIONÓW
#championStats <- data.frame(read.csv("~/TWD/Projekt 2 LOL/LOL_champions_stats.csv",sep = ';'))
championStats <- data.frame(read.csv("~/TWD/Projekt 2 LOL/LOL_champions_stats.csv"))
colnames(championStats) <- c("xd")
ChampionStats <- data.frame(name = character(),
                            type = character(),
                            playStyle = character())
for (champ in championStats$xd)
  ChampionStats <- rbind(ChampionStats, data.frame(name = unlist(strsplit(champ,';'))[1],
                                                   type = unlist(strsplit(champ,';'))[3],
                                                   playStyle = unlist(strsplit(champ,';'))[4]))
ChampionStats <- ChampionStats %>% mutate(name = ifelse(name == "S<e9>raphine","Seraphine",name)) 
ChampionStats <- ChampionStats %>% mutate(name = ifelse(name == "Wukong","MonkeyKing",name)) 
ChampionStats <- ChampionStats %>% mutate(type = ifelse(type == "Mage Artillery","Mage",type)) 
ChampionStats <- ChampionStats %>% mutate(name = ifelse(name == "Miss Fortune","MissFortune",name)) 
ChampionStats <- ChampionStats %>% mutate(type = ifelse(type == "Controllerer","Controller",type)) 
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Nilah',type = 'Slayer',playStyle = 'Skirmisher'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Akshan',type = 'Slayer',playStyle = 'Assassin'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Vex',type = 'Mage',playStyle = 'Burst'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Zeri',type = 'Marksman',playStyle = 'Marksman'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'RenataGlasc',type = 'Controller',playStyle = 'Enchanter'))
ChampionStats <- rbind(ChampionStats,data.frame(name = "Bel'Veth",type = 'Slayer',playStyle = 'Skirmisher'))
ChampionStats <- rbind(ChampionStats,data.frame(name = "K'Sante",type = 'Tank',playStyle = 'Warden'))
ChampionStats <- rbind(ChampionStats,data.frame(name = "Milio",type = 'Controller',playStyle = 'Enchanter'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Naafiri',type = 'Slayer',playStyle = 'Assassin'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Briar',type = 'Fighter',playStyle = 'Diver'))
ChampionStats <- rbind(ChampionStats,data.frame(name = 'Hwei',type = 'Mage',playStyle = 'Artillery'))
#Gnar Mega,Kled & S

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

mythicItems <- c( "Crown of the Shattered Queen",
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
mythicItemsDf <- data.frame(
  mythicItemName = mythicItems,
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
for (item in Items$data) {
  if (item$maps$'11') {
    itemsNormal <- rbind(itemsNormal, data.frame(itemId = as.numeric(gsub('.png',"",item$image$full)),
                                                 itemName = item$name,
                                                 depth = ifelse(is.null(item$depth),1,item$depth)))
  }
}
itemsNormal <- itemsNormal %>% mutate(depth = ifelse(itemId %in% as.numeric(unlist(Items$data$'1001'$into)),3,depth))

#LISTA LEGEND I MITHIC NA NORMALU
mythicItemsId <- left_join(itemsNormal %>% filter(itemName %in% mythicItems) %>% select(itemId,itemName),mythicItemsDf,join_by('itemName'=='mythicItemName'))
legendaryItemsNormal <- itemsNormal %>% filter(tolower(itemName) %in% tolower(legendary_items)) %>% mutate(mythic = ifelse(itemName %in% mythicItems,1,0)) 
boots <- c(3006,3009,3020,3047,3111,3117,3158)

ItemChampGroup <- read.csv("C:/Users/endoa/OneDrive/Dokumenty/TWD/Projekt 2 LOL/git/R-dashboard/db/playerMatchStats.csv")


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
