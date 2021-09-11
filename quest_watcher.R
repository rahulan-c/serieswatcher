# QUEST WATCHER
# Updated: 2021-09-11

# User choices ================================================================

# Enter sheet details
sheetid <- "1Y_jYuHJUnDqfYMpO3YIainspTJa7o3hgd6smeiHfe20"
sheetname <- "Raw Data"

# League parameters (unlikely to need changing)
perf <- "rapid"
clock <- 15
increment <- 10
perf_tiebreak <- "blitz"
clock_tiebreak <- 3
increment_tiebreak <- 2
perf_arma <- "blitz"
# clock_arma_w <- 5
# clock_arma_b <- 4

# Lichess API token - assumed to be saved in "api_token.txt"
token <- read.delim2("C:/Users/rahul/Documents/Github/serieswatcher/api_token.txt", header = F)[1,1]

# Packages, options and functions ---------------------------------------------
library(tidyverse)
library(googlesheets4)
library(httr)
library(lubridate)
library(jsonlite)
library(data.table)
library(stringi)
library(ndjson)
library(cli)
# library(tictoc)

# Functions -------------------------------------------------------------------
## Format search dates correctly ----------------------------------------------
LichessDateFormat <- function(date, time_modifier){
  date <- as.numeric(formatC(
    as.numeric(lubridate::parse_date_time(paste0(date, " ", time_modifier), "ymdHMS")) * 1000,
    digits = 0, format = "f"
  ))
  return(date)
}

## Update Quest pairing sheet
QuestUpdate <- function(week_choice){

  # Read pairings data from Google Sheets worksheet
  quest_pairs <- range_read(
    ss = sheetid,
    sheet = sheetname,
    range = "F:N",
    col_names = T
  )
  Sys.sleep(1)
  colnames(quest_pairs) <- c("round_start", "round_end", "week", "board",
                             "fullstring_a", "fullstring_b", "result_reported",
                             "player_a", "player_b")


  quest_pairs <- quest_pairs %>%
    mutate(sheet_row = row_number() + 1) # for updating the worksheet afterwards

  # Get the round we want
  if(is.numeric(week_choice)) {
    quest_pairs <- quest_pairs %>%
      filter(week == week_choice)
  } else if (week_choice == "latest") {
    quest_pairs <- quest_pairs %>%
      filter(week == max(week))
  }

  quest_pairs$board <- NULL
  quest_pairs$round_start <- as.character(quest_pairs$round_start)
  quest_pairs$round_end <- as.character(quest_pairs$round_end)


  # For recording pairing details
  round_pairs <- tibble(
    "round" = quest_pairs$week,
    "round_start" = quest_pairs$round_start,
    "round_end" = quest_pairs$round_end,
    "p1" = quest_pairs$player_a,
    "p2" = quest_pairs$player_b,
    "row" = quest_pairs$sheet_row,
    "rating1" = rep(NA, nrow(quest_pairs)),
    "rating2" = rep(NA, nrow(quest_pairs)),
    "res_rapid1" = rep(NA, nrow(quest_pairs)),
    "res_rapid2" = rep(NA, nrow(quest_pairs)),
    "res_rapid3" = rep(NA, nrow(quest_pairs)),
    "res_rapid4" = rep(NA, nrow(quest_pairs)),
    "res_blitz1" = rep(NA, nrow(quest_pairs)),
    "res_blitz2" = rep(NA, nrow(quest_pairs)),
    "res_arma" = rep(NA, nrow(quest_pairs)),
    "link_rapid1" = rep(NA, nrow(quest_pairs)),
    "link_rapid2" = rep(NA, nrow(quest_pairs)),
    "link_rapid3" = rep(NA, nrow(quest_pairs)),
    "link_rapid4" = rep(NA, nrow(quest_pairs)),
    "link_blitz1" = rep(NA, nrow(quest_pairs)),
    "link_blitz2" = rep(NA, nrow(quest_pairs)),
    "link_arma" = rep(NA, nrow(quest_pairs)),
    "result_computed" = rep(NA, nrow(quest_pairs))
  )


  # Get match/game data for each round pairing
  cli::cli_inform("Need data on {nrow(round_pairs)} pairings")

  for (m in seq(1:nrow(round_pairs))) {

    cli::cli_inform("{m}) {round_pairs$p1[m]}-{round_pairs$p2[m]}")

    # Query Lichess API for rapid games in that week
    query <- httr::GET(
      url = "https://lichess.org",
      path = paste0("/api/games/user/", round_pairs$p1[m]),
      query = list(
        since = LichessDateFormat(round_pairs$round_start[m], "00:00:01"),
        until = LichessDateFormat(round_pairs$round_end[m], "23:59:59"),
        perfType = perf,
        vs = round_pairs$p2[m],
        rated  = "true",
        clocks = "true"
      ),
      httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
      accept("application/x-ndjson")
    )

    Sys.sleep(2)

    # If something goes wrong
    if (query$status_code != 200) {
      print(http_status(query)$message)
      cli::cli_alert_danger("The Lichess API query returned an error. Stopping process.")
      break
    }

    # If the query is OK
    res <- query %>% httr::content("text",
                                   encoding = stringi::stri_enc_detect(httr::content(
                                     query, "raw"))[[1]][1, 1])

    # If no games found, try switching the players around in the Lichess games
    # search query. If still no luck, move to the next pairing
    if(res == ""){
      cli::cli_inform("No games found. Trying a reversed search")
      query2 <- httr::GET(
        url = "https://lichess.org",
        path = paste0("/api/games/user/", round_pairs$p2[m]),
        query = list(
          since = LichessDateFormat(round_pairs$round_start[m], "00:00:01"),
          until = LichessDateFormat(round_pairs$round_end[m], "23:59:59"),
          perfType = perf,
          vs = round_pairs$p1[m],
          rated  = "true",
          clocks = "true"
        ),
        httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
        accept("application/x-ndjson")
      )
      if (query2$status_code != 200) {
        print(http_status(query2)$message)
        cli::cli_alert_danger("The Lichess API query returned an error. Stopping process.")
        break
      }
      res2 <- query2 %>% httr::content("text",
                                     encoding = stringi::stri_enc_detect(httr::content(
                                       query2, "raw"))[[1]][1, 1])
      if(res2 == ""){
        cli::cli_inform("No games found in either player's history.")
        next
      } else {
        res <- res2
      }
    }

    # If at least one game has been returned...
    # Extract game data and see if any games match the desired time control
    res <- res %>% read_lines() %>% ndjson::flatten()
    res <- res %>%
      filter(clock.initial == clock * 60 &
               clock.increment == increment) %>%
      arrange(createdAt)

    cli::cli_inform("Found {nrow(res)} rapid games: {res$id}")

    # If no suitable rapid games are returned
    if(nrow(res) == 0) {
      cli::cli_inform("No suitable rapid games found")
      next
    }

    # If more than 4 suitable rapid games are found
    if (nrow(res) > 4) {
      cli::cli_inform("Too many suitable games played (!)")
      next
    }

    # Otherwise start identifying match data
    # 1 row per rapid game
    match_data <- tibble("game" = seq(1:nrow(res)),
                         "type" = rep("rapid", nrow(res)),
                         "id" = res$id,
                         "white" = stringr::str_to_lower(res$players.white.user.name),
                         "black" = stringr::str_to_lower(res$players.black.user.name),
                         "winner" = res$winner,
                         "status" = res$status)

    # Add rows for any unplayed games
    unplayed_rapid <- 4 - nrow(match_data)
    if(unplayed_rapid > 0) {
      match_data <- match_data %>%
        add_row(id = rep("unplayed", unplayed_rapid)) %>%
        mutate(game = seq(1:nrow(.)))
    }

    # Add both players' rapid ratings (when G1 began) to the round data
    round_pairs$rating1[m] <- ifelse(match_data$white[1] == stringr::str_to_lower(round_pairs$p1[m]),
                                     res$players.white.rating,
                                     res$players.black.rating)
    round_pairs$rating2[m] <- ifelse(match_data$black[1] == stringr::str_to_lower(round_pairs$p1[m]),
                                     res$players.white.rating,
                                     res$players.black.rating)

    # Extract game results and links
    match_data <- match_data %>%
      mutate(
        result = case_when(
          white == stringr::str_to_lower(round_pairs$p1[m]) & winner == "white" ~ "1-0",
          white == stringr::str_to_lower(round_pairs$p2[m]) & winner == "white" ~ "1-0",
          black == stringr::str_to_lower(round_pairs$p1[m]) & winner == "black" ~ "0-1",
          black == stringr::str_to_lower(round_pairs$p2[m]) & winner == "black" ~ "0-1",
          status == "draw" | status == "stalemate" ~ "1/2-1/2",
          TRUE ~ NA_character_
        )) %>%
      mutate(
        p1_pts = case_when(
          white == stringr::str_to_lower(round_pairs$p1[m]) & winner == "white" ~ 1,
          white == stringr::str_to_lower(round_pairs$p2[m]) & winner == "white" ~ 0,
          black == stringr::str_to_lower(round_pairs$p1[m]) & winner == "black" ~ 1,
          black == stringr::str_to_lower(round_pairs$p2[m]) & winner == "black" ~ 0,
          status == "draw" | status == "stalemate" ~ 0.5,
          TRUE ~ NA_real_
        )) %>%
      mutate(p2_pts = 1 - p1_pts)

    # Add results/links to round data
    round_pairs$res_rapid1[m] <- match_data$result[1]
    round_pairs$res_rapid2[m] <- match_data$result[2]
    round_pairs$res_rapid3[m] <- match_data$result[3]
    round_pairs$res_rapid4[m] <- match_data$result[4]
    round_pairs$link_rapid1[m] <- ifelse(!(is.na(round_pairs$res_rapid1[m])),
                                      glue::glue("https://lichess.org/{match_data$id[1]}"),
                                      NA)
    round_pairs$link_rapid2[m] <- ifelse(!(is.na(round_pairs$res_rapid2[m])),
                                      glue::glue("https://lichess.org/{match_data$id[2]}"),
                                      NA)
    round_pairs$link_rapid3[m] <- ifelse(!(is.na(round_pairs$res_rapid3[m])),
                                         glue::glue("https://lichess.org/{match_data$id[3]}"),
                                         NA)
    round_pairs$link_rapid4[m] <- ifelse(!(is.na(round_pairs$res_rapid4[m])),
                                         glue::glue("https://lichess.org/{match_data$id[4]}"),
                                         NA)


    # Track how many games played and overall match score
    numgames <- nrow(match_data)
    p1score <- sum(match_data$p1_pts, na.rm = T)
    p2score <- sum(match_data$p2_pts, na.rm = T)
    cli::cli_alert_info("{p1score}-{p2score} after rapid")


    # Tied matches
    if((p1score == 2) & (p2score == 2)) {

      cli::cli_inform("Match tied after rapid games. Looking for tiebreaks.")

      # Query Lichess API to find tiebreak games between when the last
      #   rapid game ended and the end of the round
      time_last_rapid <- res$lastMoveAt[nrow(match_data)]
      query_tb <- httr::GET(
        url = "https://lichess.org",
        path = paste0("/api/games/user/", round_pairs$p1[m]),
        query = list(
          since = time_last_rapid,
          until = LichessDateFormat(round_pairs$round_end[m], "23:59:59"),
          perfType = perf_tiebreak,
          vs = round_pairs$p2[m],
          rated  = "true",
          clocks = "true"
        ),
        httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
        accept("application/x-ndjson")
      )

      Sys.sleep(2)

      if (query_tb$status_code != 200) {
        print(http_status(query_tb)$message)
        cli::cli_alert_danger("The Lichess API query returned an error. Stopping process.")
        break
      }

      res_tb <- query_tb %>% httr::content("text",
                                           encoding = stringi::stri_enc_detect(httr::content(
                                             query_tb, "raw"))[[1]][1, 1])

      if(res_tb == "") {

        cli::cli_alert_warning("No possible tiebreak games found. Will try searching with the players reversed.")
        query2 <- httr::GET(
          url = "https://lichess.org",
          path = paste0("/api/games/user/", round_pairs$p2[m]),
          query = list(
            since = time_last_rapid,
            until = LichessDateFormat(round_pairs$round_end[m], "23:59:59"),
            perfType = perf_tiebreak,
            vs = round_pairs$p1[m],
            rated  = "true",
            clocks = "true"
          ),
          httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
          accept("application/x-ndjson")
        )
        if (query2$status_code != 200) {
          print(http_status(query2)$message)
          cli::cli_alert_danger("The Lichess API query returned an error. Stopping process.")
          break
        }
        res2 <- query2 %>% httr::content("text",
                                         encoding = stringi::stri_enc_detect(httr::content(
                                           query2, "raw"))[[1]][1, 1])
        if(res2 == ""){
          cli::cli_inform("No possible tiebreak games found in either player's history.")
          next
        } else {
          res_tb <- res2
        }
        } else {

      res_tb <- res_tb %>% read_lines() %>% ndjson::flatten()

      res_tb <- res_tb %>%
        filter(clock.initial == clock_tiebreak * 60 &
                 clock.increment == increment_tiebreak) %>%
        arrange(createdAt)

      cli::cli_inform("Found {nrow(res_tb)} possible tiebreak games: {res_tb$id}")

      # If there are no suitable blitz games identified (or if there are more
      # than 2), abandon the search
      if ((nrow(res_tb) == 0) || (nrow(res_tb) > 2)) {
        cli::cli_alert_warning("Zero or 3+ candidate tiebreak games identified, can't proceed")
        next
      }

      # Like for the rapid games, construct a tibble to store tiebreak game data
      tb_data <- tibble("game" = seq(1:nrow(res_tb)),
                        "type" = rep("blitz", nrow(res_tb)),
                        "id" = res_tb$id,
                        "white" = stringr::str_to_lower(res_tb$players.white.user.name),
                        "black" = stringr::str_to_lower(res_tb$players.black.user.name),
                        "winner" = res_tb$winner,
                        "status" = res_tb$status)

      # Add rows for any unplayed blitz games
      unplayed_blitz <- 4 - nrow(tb_data)
      if(unplayed_blitz > 0) {
        tb_data <- tb_data %>%
          add_row(id = rep("unplayed", unplayed_blitz)) %>%
          mutate(game = seq(1:nrow(.)))
      }

      # Extract blitz game results and links
      tb_data <- tb_data %>%
        mutate(
          result = case_when(
            white == stringr::str_to_lower(round_pairs$p1[m]) & winner == "white" ~ "1-0",
            white == stringr::str_to_lower(round_pairs$p2[m]) & winner == "white" ~ "1-0",
            black == stringr::str_to_lower(round_pairs$p1[m]) & winner == "black" ~ "0-1",
            black == stringr::str_to_lower(round_pairs$p2[m]) & winner == "black" ~ "0-1",
            status == "draw" | status == "stalemate" ~ "1/2-1/2",
            TRUE ~ NA_character_
          )) %>%
        mutate(
          p1_pts = case_when(
            white == stringr::str_to_lower(round_pairs$p1[m]) & winner == "white" ~ 1,
            white == stringr::str_to_lower(round_pairs$p2[m]) & winner == "white" ~ 0,
            black == stringr::str_to_lower(round_pairs$p1[m]) & winner == "black" ~ 1,
            black == stringr::str_to_lower(round_pairs$p2[m]) & winner == "black" ~ 0,
            status == "draw" | status == "stalemate" ~ 0.5,
            TRUE ~ NA_real_
          )) %>%
        mutate(p2_pts = 1 - p1_pts)

      # Add blitz results/links to round data
      round_pairs$res_blitz1[m] <- tb_data$result[1]
      round_pairs$res_blitz2[m] <- tb_data$result[2]

      round_pairs$link_blitz1[m] <- ifelse(!(is.na(round_pairs$res_blitz1[m])),
                                           glue::glue("https://lichess.org/{tb_data$id[1]}"),
                                           NA)
      round_pairs$link_blitz2[m] <- ifelse(!(is.na(round_pairs$res_blitz2[m])),
                                           glue::glue("https://lichess.org/{tb_data$id[2]}"),
                                           NA)

      # Report match score after identified blitz tiebreaks
      cli::cli_alert_info("{p1score + sum(tb_data$p1_pts, na.rm = T)}-{p2score + sum(tb_data$p2_pts, na.rm = T)} after rapid + blitz")

      # Check match score
      # If it's still tied, check for an armageddon game
      # TODO

        }
    }

  } # end pairing loop

  cli::cli_alert_info("Checked all pairings")

  round_pairs$result_computed <- NULL

  # Update Quest sheet with identified round pairing data
  googlesheets4::range_write(ss = sheetid, sheet = sheetname,
                             range = glue::glue("Q{round_pairs$row[1]}:AF{round_pairs$row[nrow(round_pairs)]}"),
                             data = tibble(round_pairs[,7:ncol(round_pairs)]),
                             col_names = F, reformat = T)

  cli::cli_alert_success("Updated Quest sheet with pairing details!")

} # end function



# Call function ===============================================================

## For a single round ---------------------------------------------------------
# Run QuestUpdate() for a single round
# QuestUpdate(week_choice = 2)


## For multiple rounds --------------------------------------------------------
# Update Quest sheet for multiple rounds
<<<<<<< HEAD
round_range <- c(2)
=======
round_range <- c(18:25)
>>>>>>> e422c951287d49a3ca9dee01bafc370932af9cc3
for (r in seq(1:length(round_range))) {
  QuestUpdate(week_choice = round_range[[r]])
  Sys.sleep(5)
}

# pairings that caused an error, so I've skipped those rounds entirely
# Need to go back later and investigate...and re-run for the whole round
# So far, these have all been pairings with entered match scores of 2.5-2.
# rd 10 mvl-isav | script says 2-2 after rapid, but raises error after it can't find
#                  any tiebreak games. in reality, IsaVulpes forfeited after the rapid games,
#                  which explains the match result of 2.5-2.
# rd 17: Qudit-IsaVulpes
# rd 25: jwwells42-Shnippy

# So far, has been run for weeks 1-25,
# but weeks 10, 17 and 25 need to be re-run



