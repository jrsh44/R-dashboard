# This script is used to update the playerMatchStats.csv file with new matches for a given player.

ids <- get_match_ids(api_key, puuid, 100)
match_list <- get_matches(api_key, ids, 1, 80)

existing_data <- read.csv("./db/playerMatchStats.csv")

for (i in 1:length(match_list)) {
  player_idx <- which(match_list[[i]]$metadata$participants == puuid)
  match_id <- match_list[[i]]$metadata$matchId
  kills <- match_list[[i]]$info$participants$kills[[player_idx]]
  deaths <- match_list[[i]]$info$participants$deaths[[player_idx]]
  assists <- match_list[[i]]$info$participants$assists[[player_idx]]
  win <- match_list[[i]]$info$participants$win[[player_idx]]
  total_damage_dealt_to_champions <- match_list[[i]]$info$participants$totalDamageDealtToChampions[[player_idx]]
  total_damage_taken <- match_list[[i]]$info$participants$totalDamageTaken[[player_idx]]
  gold_earned <- match_list[[i]]$info$participants$goldEarned[[player_idx]]
  champion_name <- match_list[[i]]$info$participants$championName[[player_idx]]
  champ_level <- match_list[[i]]$info$participants$champLevel[[player_idx]]
  vision_score <- match_list[[i]]$info$participants$visionScore[[player_idx]]
  nexus_kills <- match_list[[i]]$info$participants$nexusKills[[player_idx]]
  turret_kills <- match_list[[i]]$info$participants$turretKills[[player_idx]]
  inhibitor_kills <- match_list[[i]]$info$participants$inhibitorKills[[player_idx]]

  new_record <- data.frame(
    player_id = puuid,
    match_id = match_id,
    kills = kills,
    deaths = deaths,
    assists = assists,
    win = win,
    total_damage_dealt_to_champions = total_damage_dealt_to_champions,
    total_damage_taken = total_damage_taken,
    gold_earned = gold_earned,
    champ_level = champ_level,
    champion_name = champion_name,
    vision_score = vision_score,
    nexus_kills = nexus_kills,
    turret_kills = turret_kills,
    inhibitor_kills = inhibitor_kills
    )

  if (!any(duplicated(rbind(existing_data, new_record)))) {
    existing_data <- rbind(existing_data, new_record)
  }

}

write.csv(existing_data, "./db/playerMatchStats.csv", row.names = FALSE)
