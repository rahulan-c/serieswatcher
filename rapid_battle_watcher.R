
# =============================================================================
#                          RAPID BATTLE WATCHER
# =============================================================================

# PURPOSE
# Checks for completed Series games for all pairings in the current
# season. When it finds a completed 90+30 game between two paired players with
# the right colours, it updates the API worksheet in the current Series
# spreadsheet with the game result and URL, and then sends messages on Lichess
# to both players to tell them that their game has been picked up.


# ---- USER-DEFINED PARAMETERS ------------------------------------------------

# Latest Series spreadsheet ID
season_key <- "1VCA6XIJikVlakblodyfqXcy3rf7UkLPGJ5jlpi6CZ0E"
# S14 sheet (testing): "18i0HckfP063BZYZs5xqRozyqX_Y3nErWoOaiARESrvM"
# S15 sheet (curr. season): "1VCA6XIJikVlakblodyfqXcy3rf7UkLPGJ5jlpi6CZ0E"

# Season start date (YYYY-MM-DD)
season_start <- "2021-05-03" # for testing
# season_start <- "2021-08-09"

# Ranges in API sheet containing player pairings
pairing_ranges <- c("B2:E10", "G2:J10", "L2:O10") # for testing
# pairing_ranges <- c('B2:E53', 'G2:J53', 'L2:O53', 'Q2:T53', 'V2:Y53', 'AA2:AD53', 'AF2:AI53')

api_sheet <- "test" # Name of the worksheet we want to update
# "test", "API"

token <- Sys.getenv("LICHESS_TOKEN") # Lichess API token (should have msg:write scope)

# -----------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(httr)
library(lubridate)
library(jsonlite)
library(data.table)
library(stringi)
library(ndjson)
library(cli)

options(gargle_oauth_email = TRUE) # to allow non-interactive use

# Function: send a direct message on Lichess to a player to tell them that their
# game has been picked up. Needs a Lichess API token with the msg:write scope.
# The message is sent from the user's Lichess account. Also, if you send more
# than 20 new messages within a 24-hour period, you'll be rate-limited. See
# https://github.com/ornicar/lila/blob/d5ac9f2774f9da106a57ce95ad63e4213f20cd17/modules/msg/src/main/MsgSecurity.scala#L31-L53
# This only applies to messages to users you've not previously messaged.
send_message <- function(p1, p2, game_id) {

  # Define message text
  message <- sprintf(
    "The Series bot thinks it has found your game against @%s
  and has updated the current season's spreadsheet accordingly. If this was a
  mistake, please contact the Series mods to fix it. If it's correct, you don't
                     need to do anything. This is an automated message.\n\nGame ID: %s",
    p2, game_id
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

#' Updates the current Series spreadsheet with the results / links of recently
#' completed games.
#'
#' @param season_key The ID ("key") of the current Series season spreadsheet
#' @param season_start When the current season started, in YYYY-MM-DD
#' @param pairing_ranges The cell references of the ranges in the "API" sheet that have the current season pairings.
#' @param api_sheet The name of the worksheet that needs to be updated. Usually "API".
#' @param token Your Lichess API token. Must have the msg:write scope.
#'
#' @return Console messages informing you of progress. Final message will read "All done!".
#' @export
#'
#' @examples
#' series_update()
series_update <- function(season_key, season_start, pairing_ranges, api_sheet,
                          token) {

  # Read API sheet within the current season's spreadsheet
  cli::cli_alert_info("Reading all pairings from spreadsheet.")

  all_pairs <- list()
  for (r in seq(1:length(pairing_ranges))) {
    round_pairs <- range_read(
      ss = season_key,
      sheet = api_sheet,
      range = pairing_ranges[r],
      col_names = F
    )
    round_pairs$range <- r # round id
    round_pairs$order <- seq(1:nrow(round_pairs)) # pairing order
    all_pairs[[r]] <- round_pairs
  }

  all_pairs <- data.table::rbindlist(all_pairs)
  colnames(all_pairs)[1:4] <- c("white", "black", "score_w", "link")

  unplayed <- all_pairs %>%
    filter(!(is.na(white))) %>%
    filter(!(is.na(black))) %>%
    filter(is.na(link))
  unplayed$status <- rep(NA, nrow(unplayed))
  print(paste0(nrow(unplayed), " unplayed pairings to check..."))
  cli::cli_alert_info(paste0(nrow(unplayed), " unplayed pairings to check..."))

  # Search for potential Series games for each unplayed pairing
  for (p in seq(1:nrow(unplayed))) {
    query_w <- unplayed$white[p]
    query_b <- unplayed$black[p]
    print(paste0("Checking ", query_w, " - ", query_b))

    # Request games
    query <- httr::GET(
      url = "https://lichess.org",
      path = paste0("/api/games/user/", query_w),
      query = list(
        since = as.character(formatC(as.numeric(lubridate::parse_date_time(season_start, "ymd")) * 1000, digits = 0, format = "f")),
        until = "Now",
        perfType = "classical",
        vs = query_b,
        clocks = "true"
      ),
      httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
      accept("application/x-ndjson")
    )

    Sys.sleep(2)

    # Stop loop if there's an error
    if (query$status_code != 200) {
      print(http_status(query)$message)
      print("The Lichess API query returned an error. Stopping process...")
      break
    }

    # Convert response
    res <- query %>% httr::content("text", encoding = stringi::stri_enc_detect(httr::content(query, "raw"))[[1]][1, 1])

    # If games are found...
    if (res != "") {
      res <- res %>%
        read_lines() %>%
        ndjson::flatten()

      # Check how many 90+30 games they have
      res <- res %>% filter(clock.initial == 90 * 60 & clock.increment == 30)

      if (nrow(res) == 1) {
        print("Found a probable Series game!")

        # Extract White score
        score_w <- ifelse(res$status %in% c("draw", "stalemate"), 0.5, 99)
        if (score_w == 99) {
          score_w <- ifelse(res$winner == "white", 1, score_w)
          score_w <- ifelse(res$winner == "black", 0, score_w)
        }

        # Update df
        unplayed$status[p] <- "suitable 90+30 game found"
        unplayed$score_w[p] <- score_w
        unplayed$link[p] <- paste0("https://lichess.org/", res$id)
      } else if (nrow(res) > 1) {
        print("More than one potential Series game found -- need to investigate")
        unplayed$status[p] <- "multiple 90+30 games found"
        next
      } else {
        print("No suitable games found")
        unplayed$status[p] <- "no 90+30 games found"
        next
      }
    } else {
      # If games aren't found...
      print("No pairing found")
      unplayed$status[p] <- "no classical games found"
      next
    }
  }

  print("All unplayed pairings checked!")

  # Now take the pairings with likely Series games and update the Google sheet

  print("Updating the spreadsheet...")
  definites <- unplayed %>% filter(str_detect(status, "suitable"))

  if (nrow(definites) > 0) {
    for (d in seq(1:nrow(definites))) {
      score_link <- tibble("score_w" = definites$score_w[d], "link" = definites$link[d])

      range_pasted <- paste0(
        LETTERS[(definites$range[d] * 5) - 1],
        definites$order[d] + 1,
        ":",
        LETTERS[(definites$range[d] * 5)],
        definites$order[d] + 1
      )

      range_write(
        ss = season_key,
        sheet = api_sheet,
        data = score_link,
        range = range_pasted,
        col_names = F
      )
    }
    print("Spreadsheet updated!")
  } else {
    print("No updates required")
  }

  if (nrow(definites) > 0) {
    # After sheet is updated, send messages to both players
    # Only send max 24 msgs in a batch to avoid getting rate-limited
    print("Sending messages...")
    if (nrow(definites) <= 12) {
      for (d in seq(1:nrow(definites))) {
        send_message(definites$white[d], definites$black[d], str_sub(definites$link[d], start = -8))
        Sys.sleep(1)
        send_message(definites$black[d], definites$white[d], str_sub(definites$link[d], start = -8))
        Sys.sleep(1)
      }
    }
    print("All messages sent!")
  } else {
    print("No messages required")
  }

  # Then tell user about pairings with multiple poss. games
  unplayed %>%
    filter(str_detect(status, "multiple")) %>%
    select(white, black, range, order, status)

  print("All done!")
}
