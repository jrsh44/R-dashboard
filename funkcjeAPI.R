# Functions for getting data from Riot Games API

library(httr)
library(jsonlite)
library(dplyr)

keysFile <- file("./keys.txt", "r")

puuid <- readLines(keysFile, n = 1)
api_key <- readLines(keysFile, n = 1)

close(keysFile)

puuid_cwalina <- "ymtzWFkDF4IPwAb2NIiUp8G9189sByZDch0PUMeePvymtkqx-uIt0j36OT9advtmv_EhYmyjAjk5Xw" 
puuid_borycki <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
puuid_jarosz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

riot_api_request <- function(api_url, api_key) {
  response <- GET(api_url, add_headers("X-Riot-Token" = api_key))
  if (http_status(response)$category == "Success") {
    json_content <- content(response, "text", encoding = "UTF-8")
    r_list <- fromJSON(json_content)
    return(r_list)
  } else {
    stop("Error: Unable to retrieve data from the Riot Games API.")
  }
}


get_match_ids <- function(api_key, puuid, n = 100) {
  ids <-
    riot_api_request(
      paste(
        'https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/',
        puuid,
        '/ids?count=',
        as.character(n),
        sep = ''
      ),
      api_key
    )
  return(ids)
}

get_matches <- function(api_key,
                        ids,
                        min = 1,
                        max = length(ids)) {
  matchList <- list()
  for (i in min:max) {
    matchList[[i - min + 1]] <-
      riot_api_request(
        paste(
          'https://europe.api.riotgames.com/lol/match/v5/matches/',
          ids[[i]],
          sep = ''
        ),
        api_key
      )
    
  }
  return(matchList)
}


get_matches_timelines <- function(api_key,
                                  ids,
                                  min = 1,
                                  max = length(ids)) {
  matchTimelineList <- list()
  for (i in min:max) {
    matchTimelineList[[i - min + 1]] <-
      riot_api_request(
        paste(
          'https://europe.api.riotgames.com/lol/match/v5/matches/',
          ids[[i]],
          '/timeline',
          sep = ''
        ),
        api_key
      )
    
  }
  return(matchTimelineList)
}

filter_matches <- function(matchList, type) {
  # type 1=summoners rift,2=ARAM
  filtered <- list()
  c <- 1
  if (type == 1) {
    for (i in 1:length(matchList)) {
      if (matchList[[i]]$info$mapId == 11) {
        filtered[[c]] <- matchList[[i]]
        c <- c + 1
      }
    }
  } else if (type == 2) {
    for (i in 1:length(matchList)) {
      if (matchList[[i]]$info$mapId %in% c(12, 13)) {
        filtered[[c]] <- matchList[[i]]
        c <- c + 1
      }
    }
  } else{
    print("Podałeś zły typ ziomeczku patrz w opisie funkcji z farcikiem")
  }
  return(filtered)
}

filter_match_timelines <-
  function(matchTimelineList, filtered_matches) {
    # funkcja filtruje timeline zgodnie z typem gier filtered matches - aram, rift
    filtered_timelines <- list()
    c <- 1
    for (i in 1:length(filtered_matches)) {
      for (j in 1:length(matchTimelineList)) {
        if (filtered_matches[[i]]$metadata$matchId == matchTimelineList[[j]]$metadata$matchId) {
          filtered_timelines[[c]] <- matchTimelineList[[j]]
          c <- c + 1
        }
      }
    }
    return(filtered_timelines)
  }

list_to_df <- function(matchList){
  json_string <- toJSON(matchList,pretty = T)
  parsed_data <- fromJSON(json_string)
  df <- as.data.frame(do.call(rbind, parsed_data))
  return(df)
}