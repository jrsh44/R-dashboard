# This script is used to update the matches.csv file with new matches for a given player.

ids <- get_match_ids(api_key, puuid, 100)
matchList <- get_matches(api_key, ids, 1, 20)

existingData <- read.csv("./db/matches.csv")

for (i in 1:length(matchList)) {
  matchId <- matchList[[i]]$metadata$matchId
  gameCreation <- matchList[[i]]$info$gameCreation
  gameMode <- matchList[[i]]$info$gameMode
  mapId <- matchList[[i]]$info$mapId
  paricipants <- matchList[[i]]$metadata$participants

  newRecord <- data.frame(
      playerId = puuid,
      matchId = matchId, 
      gameCreation = gameCreation, 
      gameMode = gameMode,
      mapId = mapId,
      paricipants = paricipants
  )

  if (!any(duplicated(rbind(existingData, newRecord)))) {
    existingData <- rbind(existingData, newRecord)
  }
}

write.csv(existingData, "./db/matches.csv", row.names = FALSE)