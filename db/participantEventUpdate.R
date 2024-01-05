# This script is used to update the participantEvents.csv file with new matches for a given player.

ids <- get_match_ids(api_key, puuid, 100)
match_timelines_list <- get_matches_timelines(api_key, ids, 1, 20)
matchList <- get_matches(api_key, ids, 1, 20)

matchList <- filter_matches(matchList, 1)
match_timelines_list <-
  filter_match_timelines(match_timelines_list, matchList)

existing_data <- read.csv("./db/participantEvents.csv")

#existing_data <- data.frame(
#  player_id = vector(),
#  match_id = vector(),
#  x = vector(),
#  y = vector(),
#  type = vector(),
#  minute = vector(),
#  timestamp = vector(),
# champion_id = vector(),
#  champion_name = vector(),
#  position = vector(),
#  team_id = vector(),
#  win = vector()
#)

for (i in 1:length(match_timelines_list)) {
  player_idx <-
    which(match_timelines_list[[i]]$metadata$participants == puuid)
  match_id <- match_timelines_list[[i]]$metadata$matchId
  champion_name <- matchList[[i]][["info"]][["participants"]][["championName"]][[player_idx]]
  champion_id <- matchList[[i]][["info"]][["participants"]][["championId"]][[player_idx]]
  position <- matchList[[i]][["info"]][["participants"]][["teamPosition"]][[player_idx]]
  team_id <- matchList[[i]][["info"]][["participants"]][["teamId"]][[player_idx]]
  win <- matchList[[i]][["info"]][["participants"]][["win"]][[player_idx]]
  
  for (j in 1:length(match_timelines_list[[i]][["info"]][["frames"]][["events"]])) {
    if (!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['killerId']])) {
      for (k in 1:length(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['killerId']])) {
        if (!is.na(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['killerId']][[k]])) {
          if (match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['killerId']][[k]] ==
              player_idx) {
            #print(paste(as.character(i),as.character(j),as.character(k),sep=" "))
            if(!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]])){
              x <- match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]][["x"]][[k]]
              y <- match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]][["y"]][[k]]}else{
                x <- NA
                y <- NA
              }
            new_record <- data.frame(
              player_id = puuid,
              match_id = match_id,
              x = x,
              y = y,
              type = "kill",
              minute = j,
              timestamp = match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["timestamp"]][[k]],
              champion_id = champion_id,
              champion_name = champion_name,
              position = position,
              team_id = team_id,
              win = win
            )
            
            if (!any(duplicated(rbind(existing_data, new_record)))) {
              existing_data <- rbind(existing_data, new_record)
            }
          }
        }
      }
    }
    for (k in 1:length(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['victimId']])) {
      if (!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['victimId']])) {
        if (!is.na(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['victimId']][[k]])) {
          if (match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][['victimId']][[k]] ==
              player_idx) {
            #print(paste(as.character(i),as.character(j),as.character(k),sep=" "))
            if(!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]])){
              x <- match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]][["x"]][[k]]
              y <- match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]][["y"]][[k]]}else{
                x <- NA
                y <- NA
              }
            new_record <- data.frame(
              player_id = puuid,
              match_id = match_id,
              x = x,
              y = y,
              type = "death",
              minute = j,
              timestamp = match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["timestamp"]][[k]],
              champion_id = champion_id,
              champion_name = champion_name,
              position = position,
              team_id = team_id,
              win = win
            )
            
            if (!any(duplicated(rbind(existing_data, new_record)))) {
              existing_data <- rbind(existing_data, new_record)
            }
          }
        }
      }
    }
    if (!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["assistingParticipantIds"]])) {
      for (k in 1:length(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["assistingParticipantIds"]])) {
        if (!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["assistingParticipantIds"]][[k]])) {
          if (player_idx %in% match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["assistingParticipantIds"]][[k]]) {
            #print(paste(as.character(i),as.character(j),as.character(k),sep=" "))
            if(!is.null(match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]])){
            x <- match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]][["x"]][[k]]
            y <- match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["position"]][["y"]][[k]]}else{
              x <- NA
              y <- NA
            }
            new_record <- data.frame(
              player_id = puuid,
              match_id = match_id,
              x = x,
              y = y,
              type = "assist",
              minute = j,
              timestamp = match_timelines_list[[i]][["info"]][["frames"]][["events"]][[j]][["timestamp"]][[k]],
              champion_id = champion_id,
              champion_name = champion_name,
              position = position,
              team_id = team_id,
              win = win
            )
            if (!any(duplicated(rbind(existing_data, new_record)))) {
              existing_data <- rbind(existing_data, new_record)
            }
          }
        }
      }
    }
  }
}

write.csv(existing_data, "./db/participantEvents.csv", row.names = FALSE)
