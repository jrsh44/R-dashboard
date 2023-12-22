# This script is used to update the participantFrames.csv file with new matches for a given player.

ids <- get_match_ids(api_key, puuid, 100)
matchTimelinesList <- get_matches_timelines(api_key, ids, 1, 20)

existingData <- read.csv("./db/participantFrames.csv")

for (i in 1:length(matchTimelinesList)) {
  playerIdx <- which(matchTimelinesList[[i]]$metadata$participants == puuid)
  matchId <- matchTimelinesList[[i]]$metadata$matchId
  for(j in 1:length(matchTimelinesList[[i]]$info$frames$participantFrames[[as.character(playerIdx)]]$currentGold)){
    currentGold <- matchTimelinesList[[i]]$info$frames$participantFrames[[as.character(playerIdx)]]$currentGold[[j]]
    level <- matchTimelinesList[[i]]$info$frames$participantFrames[[as.character(playerIdx)]]$level[[j]]
    xp <- matchTimelinesList[[i]]$info$frames$participantFrames[[as.character(playerIdx)]]$xp[[j]]

    newRecord <- data.frame(
        playerId = puuid,
        matchId = matchId, 
        minute = j,
        currentGold = currentGold,
        level = level,
        xp = xp
    )

    if (!any(duplicated(rbind(existingData, newRecord)))) {
      existingData <- rbind(existingData, newRecord)
    }
  }
  
}

write.csv(existingData, "./db/participantFrames.csv", row.names = FALSE)