# This script is used to update the playerMatchStats.csv file with new matches for a given player.

ids <- get_match_ids(api_key, puuid, 100)
matchList <- get_matches(api_key, ids, 1, 20)

existingData <- read.csv("./db/playerMatchStats.csv")

for (i in 1:length(matchList)) {
  playerIdx <- which(matchList[[i]]$metadata$participants == puuid)
  matchId <- matchList[[i]]$metadata$matchId
  kills <- matchList[[i]]$info$participants$kills[[playerIdx]]
  deaths <- matchList[[i]]$info$participants$deaths[[playerIdx]]
  assists <- matchList[[i]]$info$participants$assists[[playerIdx]]
  win <- matchList[[i]]$info$participants$win[[playerIdx]]
  totalDamageDealtToChampions <- matchList[[i]]$info$participants$totalDamageDealtToChampions[[playerIdx]]
  totalDamageTaken <- matchList[[i]]$info$participants$totalDamageTaken[[playerIdx]]
  goldEarned <- matchList[[i]]$info$participants$goldEarned[[playerIdx]]
  championName <- matchList[[i]]$info$participants$championName[[playerIdx]]
  champLevel <- matchList[[i]]$info$participants$champLevel[[playerIdx]]
  visionScore <- matchList[[i]]$info$participants$visionScore[[playerIdx]]
  nexusKills <- matchList[[i]]$info$participants$nexusKills[[playerIdx]]
  turretKills <- matchList[[i]]$info$participants$turretKills[[playerIdx]]
  inhibitorKills <- matchList[[i]]$info$participants$inhibitorKills[[playerIdx]]

  newRecord <- data.frame(
    playerId = puuid,
    matchId = matchId,
    kills = kills,
    deaths = deaths,
    assists = assists,
    win = win,
    totalDamageDealtToChampions = totalDamageDealtToChampions,
    totalDamageTaken = totalDamageTaken,
    goldEarned = goldEarned,
    champLevel = champLevel,
    championName = championName,
    visionScore = visionScore,
    nexusKills = nexusKills,
    turretKills = turretKills,
    inhibitorKills = inhibitorKills
    )

  if (!any(duplicated(rbind(existingData, newRecord)))) {
    existingData <- rbind(existingData, newRecord)
  }

}

write.csv(existingData, "./db/playerMatchStats.csv", row.names = FALSE)
