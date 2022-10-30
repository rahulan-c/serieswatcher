# QUEST WATCHER
# Last updated: 2021-09-11

# User choices ================================================================

# Enter sheet details
sheetid <- quest_sheetid
sheetname <- quest_sheetname

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


## Update Quest pairing sheet
QuestUpdate <- function(week_choice){

  # Read pairings data from Google Sheets worksheet
  quest_pairs <- range_read(
    ss = sheetid,
    sheet = sheetname,
    range = "F:AH",
    col_names = T
  )
  Sys.sleep(1)
  colnames(quest_pairs)[1:29] <- c("round_start", "round_end", "week", "board",
                             "fullstring_a", "fullstring_b", "result_reported",
                             "player_a", "player_b", "level_a", "level_b",
                             "rating_a", "rating_b", "result_rapid1", "result_rapid2",
                             "result_rapid3", "result_rapid4", "result_blitz1",
                             "result_blitz2", "result_arma", "link_rapid1",
                             "link_rapid2", "link_rapid3", "link_rapid4",
                             "link_blitz1", "link_blitz2", "link_arma",
                             "result_computed", "match_completed")


  quest_pairs <- quest_pairs %>%
    as_tibble() %>%
    mutate(sheet_row = row_number() + 1) # for updating the worksheet afterwards

  # Get the round we want
  if(is.numeric(week_choice)) {
    selected_week <- week_choice
    cli::cli_inform("Checking Quest pairings for Round {week_choice}...")
    quest_pairs <- quest_pairs %>%
      dplyr::filter(week == selected_week)
  } else if (week_choice == "latest") {
    cli::cli_inform("Checking Quest pairings for Round {max(quest_pairs$week)} (latest)...")
    quest_pairs <- quest_pairs %>%
      dplyr::filter(week == max(week))
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
    "rating1" = quest_pairs$rating_a,
    "rating2" = quest_pairs$rating_b,
    "res_rapid1" = quest_pairs$result_rapid1,
    "res_rapid2" = quest_pairs$result_rapid2,
    "res_rapid3" = quest_pairs$result_rapid3,
    "res_rapid4" = quest_pairs$result_rapid4,
    "res_blitz1" = quest_pairs$result_blitz1,
    "res_blitz2" = quest_pairs$result_blitz2,
    "res_arma" = quest_pairs$result_arma,
    "link_rapid1" = quest_pairs$link_rapid1,
    "link_rapid2" = quest_pairs$link_rapid2,
    "link_rapid3" = quest_pairs$link_rapid3,
    "link_rapid4" = quest_pairs$link_rapid4,
    "link_blitz1" = quest_pairs$link_blitz1,
    "link_blitz2" = quest_pairs$link_blitz2,
    "link_arma" = quest_pairs$link_arma,
    "result_computed" = quest_pairs$result_computed,
    "match_completed" = quest_pairs$match_completed
  )

  already_completed <- round_pairs %>%
    filter(match_completed == "y") %>%
    nrow()


  # Get match/game data for each round pairing
  cli::cli_inform("Quest R{round_pairs$round[1]}: about to check {nrow(round_pairs)} pairings")

  for (m in seq(1:nrow(round_pairs))) {

    # cli::cli_inform("{m}/{nrow(round_pairs)} {round_pairs$p1[m]}-{round_pairs$p2[m]}")

    # Ignore if the match has already been completed, or if the match score in
    # the Quest sheet is listed as 3-3
    if (round_pairs$match_completed[m] %in% c("y", "arma")) {
      # cli::cli_alert_info("Completed and previously recorded in sheet.")
      next
    } else if (round_pairs$result_computed[m] %in% c("3-3")) {
      # cli::cli_alert_info("Completed and previously recorded in sheet.")
      next
    }

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
      cli::cli_alert_danger("Lichess API query error (200) in initial search. Moving to next pairing...")
      next
    }

    # If the query is OK
    res <- query %>% httr::content("text",
                                   encoding = stringi::stri_enc_detect(httr::content(
                                     query, "raw"))[[1]][1, 1])

    # If no games found, try switching the players around in the Lichess games
    # search query. If still no luck, move to the next pairing
    if(res == ""){
      # cli::cli_inform("No games found. Trying a reversed search")

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

      Sys.sleep(2)

      if (query2$status_code != 200) {
        print(http_status(query2)$message)
        cli::cli_alert_danger("Lichess API query error (200) when reversing player order. Moving to next pairing...")
        next
      }
      res2 <- query2 %>% httr::content("text",
                                     encoding = stringi::stri_enc_detect(httr::content(
                                       query2, "raw"))[[1]][1, 1])
      if(res2 == ""){
        # cli::cli_inform("No games found in either player's history.")
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
      # cli::cli_inform("No suitable rapid games found")
      next
    }

    # If more than 4 suitable rapid games are found
    if (nrow(res) > 4) {
      # cli::cli_inform("Too many suitable games played (!)")
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
    if(!("winner" %in% names(match_data))){
      match_data <- match_data %>%
        mutate(result = "1/2-1/2",
               p1_pts = 0.5,
               p2_pts = 0.5)

    } else {

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

    }

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
    # cli::cli_alert_info("{p1score}-{p2score} after rapid")
    round_pairs$result_computed[m] <- glue::glue("{p1score}-{p2score}")

    # If a winner can be identified, record the match as completed
    if(max(p1score, p2score) >= 2.5) {
      round_pairs$match_completed[m] <- "y"
    }


    # Tied matches
    if((p1score == 2) & (p2score == 2)) {

      # cli::cli_inform("Match tied after rapid games. Looking for tiebreaks.")

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
        cli::cli_alert_danger("Lichess API query error (200) when looking for tiebreak games. Moving to next pairing...")
        next
      }

      res_tb <- query_tb %>% httr::content("text",
                                           encoding = stringi::stri_enc_detect(httr::content(
                                             query_tb, "raw"))[[1]][1, 1])

      if(res_tb == "") {

        # cli::cli_alert_warning("No possible tiebreak games found. Will try searching with the players reversed.")
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

        Sys.sleep(2)

        if (query2$status_code != 200) {
          print(http_status(query2)$message)
          cli::cli_alert_danger("Lichess API query error (200) when reversing player order (in tiebreak search). Moving to next pairing...")
          next
        }
        res2 <- query2 %>% httr::content("text",
                                         encoding = stringi::stri_enc_detect(httr::content(
                                           query2, "raw"))[[1]][1, 1])
        if(res2 == ""){
          # cli::cli_inform("No possible tiebreak games found in either player's history.")
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

      # cli::cli_inform("Found {nrow(res_tb)} possible tiebreak games: {res_tb$id}")

      # If there are no suitable blitz games identified (or if there are more
      # than 2), abandon the search
      if ((nrow(res_tb) == 0) || (nrow(res_tb) > 2)) {
        # cli::cli_alert_warning("Zero or 3+ candidate tiebreak games identified, can't proceed")
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

      if(!("winner" %in% names(tb_data))){
        tb_data <- tb_data %>% mutate(result = "1/2-1/2")
      } else {
        tb_data <- tb_data %>%
          mutate(
            result = case_when(
              status == "draw" | status == "stalemate" ~ "1/2-1/2",
              white == stringr::str_to_lower(round_pairs$p1[m]) & winner == "white" ~ "1-0",
              white == stringr::str_to_lower(round_pairs$p2[m]) & winner == "white" ~ "1-0",
              black == stringr::str_to_lower(round_pairs$p1[m]) & winner == "black" ~ "0-1",
              black == stringr::str_to_lower(round_pairs$p2[m]) & winner == "black" ~ "0-1",
              TRUE ~ NA_character_
            )) %>%
          mutate(
            p1_pts = case_when(
              status == "draw" | status == "stalemate" ~ 0.5,
              white == stringr::str_to_lower(round_pairs$p1[m]) & winner == "white" ~ 1,
              white == stringr::str_to_lower(round_pairs$p2[m]) & winner == "white" ~ 0,
              black == stringr::str_to_lower(round_pairs$p1[m]) & winner == "black" ~ 1,
              black == stringr::str_to_lower(round_pairs$p2[m]) & winner == "black" ~ 0,
              TRUE ~ NA_real_
            )) %>%
          mutate(p2_pts = 1 - p1_pts)
      }

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
      p1score <- p1score + sum(tb_data$p1_pts, na.rm = T)
      p2score <- p2score + sum(tb_data$p2_pts, na.rm = T)
      # cli::cli_alert_info("{p1score}-{p2score} after rapid + blitz")
      round_pairs$result_computed[m] <- glue::glue("{p1score}-{p2score}")

      # If a clear winner can be identified, record the match as completed and
      # store the match result
      if((p1score > p2score) || (p2score > p1score)) {
        round_pairs$match_completed[m] <- "y"
      } else if((p1score == 3) & (p2score == 3)){
        round_pairs$match_completed[m] <- "arma"
      }

      # Check match score
      # If it's still tied, check for an armageddon game
      # TODO

        }
    }

    # Report one-line summary per pairing
    # cli::cli_alert_info("{m}/{nrow(round_pairs)} {round_pairs$p1[m]}-{round_pairs$p2[m]}")

  } # end pairing loop

  cli::cli_alert_info("Checked all pairings")

  # round_pairs$result_computed <- NULL

  # Update Quest sheet with identified round pairing data
  googlesheets4::range_write(ss = sheetid, sheet = sheetname,
                             range = glue::glue("Q{round_pairs$row[1]}:AH{round_pairs$row[nrow(round_pairs)]}"),
                             data = tibble(round_pairs[,7:ncol(round_pairs)]),
                             col_names = F, reformat = T)

  cli::cli_alert_success("Updated Quest sheet")

  newly_completed <- round_pairs %>%
    filter(match_completed == "y") %>%
    nrow()
  newly_completed <- newly_completed - already_completed

  unplayed <- round_pairs %>%
    filter(is.na(res_rapid1)) %>%
    nrow()

  cli::cli_inform("")
  cli::cli_inform("--- QUEST SUMMARY ---")

  # Summarise update outcomes
  cli::cli_dl(c(
    "Current Quest round" = round_pairs$round[1],
    "Total pairings" = nrow(round_pairs),
    "Previously completed" = already_completed,
    "Newly completed" = newly_completed,
    "Unplayed" = unplayed
  )
  )
  cli::cli_inform("")

} # end function



# Call function ===============================================================

## Update Quest result for a single weeks/rounds ------------------------------
# Not run
# QuestUpdate(week_choice = 2)


## Update Quest results for multiple weeks/rounds -----------------------------
# Not run
# round_range <- c(35)
# for (r in seq(1:length(round_range))) {
#   QuestUpdate(week_choice = round_range[[r]])
#   Sys.sleep(5)
# }


## Update Quest results for the latest week/round -----------------------------
QuestUpdate(week_choice = "latest")

# week_choice: either numeric (week_choice = 43)
#              or "latest" (checks the highest round number listed in the "Results" sheet)


