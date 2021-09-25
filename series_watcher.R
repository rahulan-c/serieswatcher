# SERIES WATCHER
# Last updated: 2021-09-25

# Notes:
# - Only 90+30 games will get picked up.
# - Doesn't check the players' colours
# - Doesn't care if the game was rated or not.
# - Will send messages to all players of newly identified games; isn't tuned
#   to watch out for Lichess message limits.


options(
  gargle_oauth_email = TRUE, # to allow non-interactive use
  googlesheets4_quiet = TRUE
) # gs4 has verbose output enabled by default



# Notify players on Lichess that their game has been picked up
# Messages are sent from the account associated with the Lichess API token
send_message <- function(p1, p2, game_id, token) {

  # Define message text
  message <- sprintf(
    "%s Series game logged %s

    FYI your 90+30 game with @%s has just been added to the Series spreadsheet. If this was a mistake, please contact the Series mods. If it's correct, you don't need to do anything.

    %s",
    emo::ji("abacus"), emo::ji("beetle"), p2, paste0("https://lichess.org/", game_id)
  )

  # Send message using Lichess API
  # Requires an API token with the msg:write scope
  send <- httr::POST(
    url = paste0("https://lichess.org/inbox/", p1),
    body = list(text = message),
    httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
    encode = "form"
  )
  return(send)
}



# Function that checks all pairings in the Series sheet, checks for games,
# updates the sheet with any new games' links and results, and messages both
# players
SeriesUpdate <- function(season_sheetkey,         # Key/ID of Series Google Sheet
                         results_sheet = "API",   # Name of worksheet with pairing details, usually "API". All other sheets are ignored.
                         season_start,            # Season start date ("YYYY-MM-DD")
                         season_end = "Now",      # Season end date ("YYYY-MM-DD"), defaults to the current date and time.
                         pairing_ranges,          # The cell ranges in the pairing sheet (results_sheet) that contain player names
                         token = token) {         # Lichess API token (must have msg:write privileges)
  options(
    gargle_oauth_email = TRUE,
    googlesheets4_quiet = TRUE
  )

  # Define season dates for games search
  season_start <- as.character(formatC(
    as.numeric(lubridate::parse_date_time(season_start, "ymd")) * 1000,
    digits = 0, format = "f"
  ))

  # End date
  if (season_end != "Now") {
    season_end <- as.character(formatC(
      as.numeric(lubridate::parse_date_time(season_end, "ymd")) * 1000,
      digits = 0, format = "f"
    ))
  } else {
    # Or use the current date and time
    season_end <- as.character(formatC(
      as.numeric(lubridate::parse_date_time(Sys.time(), "ymdHMS")) * 1000,
      digits = 0, format = "f"
    ))
  }

  all_pairs <- list()
  for (r in seq(1:length(pairing_ranges))) {
    round_pairs <- range_read(
      ss = season_sheetkey,
      sheet = results_sheet,
      range = pairing_ranges[r],
      col_names = F
    )
    round_pairs$range <- r # round id
    round_pairs$order <- seq(1:nrow(round_pairs)) # pairing order
    all_pairs[[r]] <- round_pairs
  }

  all_pairs <- data.table::rbindlist(all_pairs)
  colnames(all_pairs)[1:4] <- c("white", "black", "score_w", "link")

  all_pairs$status <- rep(NA, nrow(all_pairs))

  # Search for potential Series games for each unplayed pairing
  for (p in seq(1:nrow(all_pairs))) {

    # Report the pairing being checked
    query_w <- all_pairs$white[p]
    query_b <- all_pairs$black[p]

    # If there's already a game and result recorded, ignore and move to the next one
    if ((!(is.na(all_pairs$link[p]))) || (!(is.na(all_pairs$score_w[p])))) {
      all_pairs$status[p] <- "old"
      next
    }

    # If one of the players is NA or blank, ignore and move on
    if ((is.na(all_pairs$white[p])) || (all_pairs$white[p] == "") ||
        (is.na(all_pairs$black[p])) || (all_pairs$black[p] == "")
    ) {
      all_pairs$status[p] <- "invalid"
      next
    }

    # Otherwise, look for suitable games between the two players
    if (p > 1) {
      Sys.sleep(2)
    }
    query <- httr::GET(
      url = "https://lichess.org",
      path = paste0("/api/games/user/", query_w),
      query = list(
        since = season_start,
        until = season_end,
        perfType = "classical",
        color = "white",
        vs = query_b,
        clocks = "true"
      ),
      httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
      accept("application/x-ndjson")
    )

    # Stop loop if there's an error
    if (query$status_code != 200) {
      print(http_status(query)$message)
      cli::cli_alert_danger("The Lichess API query returned an error. Stopping process.")
      break
    }

    # Convert response
    res <- query %>% httr::content("text", encoding = stringi::stri_enc_detect(httr::content(query, "raw"))[[1]][1, 1])

    # If classical games are found...
    if (res != "") {
      res <- res %>%
        read_lines() %>%
        ndjson::flatten()

      # Only select 90+30 games
      res <- res %>% filter(clock.initial == 90 * 60 & clock.increment == 30)


      # If only one suitable 90+30 game found, assume it's their Series game
      if (nrow(res) == 1) {

        # Extract White score
        score_w <- ifelse(res$status %in% c("draw", "stalemate"), 0.5, 99)
        if (score_w == 99) {
          score_w <- ifelse(res$winner == "white", 1, score_w)
          score_w <- ifelse(res$winner == "black", 0, score_w)
        }

        # Update pairings data
        all_pairs$status[p] <- "found"
        all_pairs$score_w[p] <- score_w
        all_pairs$link[p] <- paste0("https://lichess.org/", res$id)

        send_message(all_pairs$white[p], all_pairs$black[p], str_sub(all_pairs$link[p], start = -8),
                     token = token)
        Sys.sleep(1)
        send_message(all_pairs$black[p], all_pairs$white[p], str_sub(all_pairs$link[p], start = -8),
                     token = token)
        Sys.sleep(1)


        # If more than one suitable 90+30 game found...
      } else if (nrow(res) > 1) {
        all_pairs$status[p] <- "multiple"
        Sys.sleep(0.2)
        next

        # If no suitable 90+30 games found...
      } else {
        all_pairs$status[p] <- "none (series)"
        Sys.sleep(0.2)
        next
      }

      # If no suitable classical games found...
    } else {
      all_pairs$status[p] <- "none (classical)"
      Sys.sleep(0.2)
      next
    }
  }

  # Compile data for updating sheet
  update_data <- all_pairs %>% group_split(range, .keep = T)

  # Update sheet by pairing range
  for (r in seq(1:length(update_data))) {
    range_write(
      ss = season_sheetkey,
      sheet = results_sheet,
      data = update_data[[r]][, 1:4],
      range = pairing_ranges[r],
      col_names = F
    )
    Sys.sleep(2)
  }

  # Report how many pairings were checked and updated
  total <- nrow(all_pairs)
  invalid <- all_pairs %>% filter(status == "invalid") %>% nrow()
  old <- all_pairs %>% filter(status == "old") %>% nrow()
  unplayed <- all_pairs %>% filter(str_detect(status, "none")) %>% nrow()
  found <- all_pairs %>% filter(status == "found") %>% nrow()

  cli::cli_dl(c(
    "Total pairings" = total - invalid,
    "Previously recorded" = old,
    "Newly updated" = found,
    "Unplayed" = unplayed
  )
  )
}
