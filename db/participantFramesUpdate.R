# This script is used to update the participantFrames.csv file with new matches for a given player.

ids <- get_match_ids(api_key, puuid, 100)
match_timelines_list <- get_matches_timelines(api_key, ids, 1, 20)

existing_data <- read.csv("./db/participantFrames.csv")

for (i in 1:length(match_timelines_list)) {
  player_idx <- which(match_timelines_list[[i]]$metadata$participants == puuid)
  match_id <- match_timelines_list[[i]]$metadata$matchId
  for(j in 1:length(match_timelines_list[[i]]$info$frames$participantFrames[[as.character(player_idx)]]$currentGold)){
    current_gold <- match_timelines_list[[i]]$info$frames$participantFrames[[as.character(player_idx)]]$currentGold[[j]]
    level <- match_timelines_list[[i]]$info$frames$participantFrames[[as.character(player_idx)]]$level[[j]]
    xp <- match_timelines_list[[i]]$info$frames$participantFrames[[as.character(player_idx)]]$xp[[j]]

    new_record <- data.frame(
        player_id = puuid,
        match_id = match_id, 
        minute = j,
        current_gold = current_gold,
        level = level,
        xp = xp
    )

    if (!any(duplicated(rbind(existing_data, new_record)))) {
      existing_data <- rbind(existing_data, new_record)
    }
  }
  
}

write.csv(existing_data, "./db/participantFrames.csv", row.names = FALSE)