
# =============================================================================
#                          SERIES WATCHER
# =============================================================================

# PURPOSE
# Updates the "API" sheet in the current Series spreadsheet games between players
# paired against each other in the current Series season. When it finds such a
# game it updates the "API" worksheet in the current Series
# spreadsheet with the game result and URL, before notifying both players on
# Lichess that their game has been picked up by the "bot", so that they can
# contact the Series mods in case it's made a mistake.

# CAVEATS
# - Games must be 90+30 to be picked up.
# - The script doesn't check if the players had the right colours, or if the
#   game was rated.
# - Not currently designed to safely work around Lichess message-sending rate
#   limits. In other words, if the script is used to send more than 25 Lichess
#   DMs to "new" players (ie users who have never messaged you)

# Last updated: 2021-09-01

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

options(
  gargle_oauth_email = TRUE, # to allow non-interactive use
  googlesheets4_quiet = TRUE
) # gs4 has verbose output enabled by default

# Lichess API token - assumed to be saved in "api_token.txt"
# Needs to be loaded like this instead of from .Renviron to enable automation
token <- read.delim2("api_token.txt", header = F)[1,1]

# Sends a DM on Lichess to a player to tell them that their
# game has been picked up. Needs a Lichess API token with the msg:write scope.
# The message is sent from *your* Lichess account. Be careful - sending more than
# 25 messages in a day to users who you haven't messaged before will get you rate-
# limited...and your messages won't go through.
send_message <- function(p1, p2, game_id, token) {

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
#' @param season_sheetkey The ID ("key") of the current Series season spreadsheet
#' @param season_start When the current season started, in YYYY-MM-DD
#' @param pairing_ranges The cell references of the ranges in the "API" sheet that have the current season pairings.
#' @param results_sheet The name of the worksheet that needs to be updated. Usually "API".
#' @param token Your Lichess API token. Must have the msg:write scope. Defaults to an (assumed) RStudio environment variable called "LICHESS_TOKEN".
#'
#' @return Console messages informing you of progress. Final message will read "All done!".
#' @export
#'
#' @examples
#' series_update()
SeriesUpdate <- function(season_sheetkey, # Google Sheets "key" [ID]
                         results_sheet = "API", # sheet name with results/links to be updated, usually "API"
                         season_start, # "YYYY-MM-DD", eg "2021-05-03" for 3 May 2021
                         season_end = "Now", # Season end date, same format as season_start. Defaults to current date and time.
                         pairing_ranges, # Cell ranges in results_sheets that contain paired player names
                         token = token) {
  options(
    gargle_oauth_email = TRUE, # to allow non-interactive use
    googlesheets4_quiet = TRUE
  ) # gs4 has verbose output enabled by default


  cli::rule(line = ":", right = paste0("Update started: ",  as.character(Sys.time())))

  cli::cli_h1("Series Watcher")

  # Define season dates for games search
  # Start date
  season_start <- as.character(formatC(
    as.numeric(lubridate::parse_date_time(season_start, "ymd")) * 1000,
    digits = 0, format = "f"
  ))

  # End date
  if (season_end != "Now") {
    # If user has entered a season end date, use that to define the relevant search period for games
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

  # Get pairing names
  cli::cli_h2("Get pairing data")


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

  cli::cli_h2("Looking for games")

  cli::cli_alert_info(paste0(nrow(all_pairs), " pairings to check..."))
  sb <- cli_status("{symbol$arrow_right} Checking pairing...")

  # Search for potential Series games for each unplayed pairing
  for (p in seq(1:nrow(all_pairs))) {

    # Report the pairing being checked
    query_w <- all_pairs$white[p]
    query_b <- all_pairs$black[p]

    cli_status_update(
      id = sb,
      "{symbol$arrow_right} ({p}) {query_w}-{query_b}"
    )

    # If there's already a game and result recorded, ignore and move to the next one
    if ((!(is.na(all_pairs$link[p]))) || (!(is.na(all_pairs$score_w[p])))) {
      cli_status_update(
        id = sb,
        "{symbol$tick} ({p}) Already played: {all_pairs$link[p]} ({all_pairs$score_w[p]}-{1-all_pairs$score_w[p]})"
      )
      all_pairs$status[p] <- "old"
      next
    }

    # If one of the players is NA or blank, ignore and move on
    if ((is.na(all_pairs$white[p])) || (all_pairs$white[p] == "") ||
      (is.na(all_pairs$black[p])) || (all_pairs$black[p] == "")
    ) {
      cli_status_update(
        id = sb,
        "{symbol$tick} ({p}) Invalid"
      )
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
        cli_status_update(
          id = sb,
          "{symbol$tick} ({p}) Found! {res$id}"
        )

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

        # Notify both players on Lichess that their game was picked up
        cli::cli_status_update(
          id = sb,
          "{symbol$arrow_right} ({p}) Sending Lichess DMs to both players"
        )
        send_message(all_pairs$white[p], all_pairs$black[p], str_sub(all_pairs$link[p], start = -8),
                     token = token)
        Sys.sleep(1)
        send_message(all_pairs$black[p], all_pairs$white[p], str_sub(all_pairs$link[p], start = -8),
                     token = token)
        cli::cli_status_update(
          id = sb,
          "{symbol$tick} ({p}) DMs sent"
        )
        Sys.sleep(1)


        # If more than one suitable 90+30 game found...
      } else if (nrow(res) > 1) {
        cli_status_update(
          id = sb,
          "{symbol$cross} ({p}) Too many suitable games found: {str_c(res$id, collapse = ' ')}"
        )
        all_pairs$status[p] <- "multiple"
        Sys.sleep(0.2)
        next

        # If no suitable 90+30 games found...
      } else {
        cli_status_update(
          id = sb,
          "{symbol$cross} ({p}) No suitable games found"
        )
        all_pairs$status[p] <- "none (series)"
        Sys.sleep(0.2)
        next
      }

      # If no suitable classical games found...
    } else {
      cli_status_update(
        id = sb,
        "{symbol$cross} ({p}) No suitable games found"
      )
      all_pairs$status[p] <- "none (classical)"
      Sys.sleep(0.2)
      next
    }
  }

  cli::cli_status_clear(id = sb)
  cli::cli_alert_success("All pairings checked!")

  # Update the Google sheet with the new information
  cli::cli_h2("Updating the spreadsheet")

  # Compile data for updating sheet
  update_data <- all_pairs %>% group_split(range, .keep = T)

  cli::cli_alert_info("About to update {length(update_data)} ranges")
  sb <- cli_status("{symbol$arrow_right} Range updated")

  # Update sheet by pairing range
  for (r in seq(1:length(update_data))) {
    range_write(
      ss = season_sheetkey,
      sheet = results_sheet,
      data = update_data[[r]][, 1:4],
      range = pairing_ranges[r],
      col_names = F
    )
    cli_status_update(
      id = sb,
      "{symbol$tick} {r}/{length(update_data)} ranges updated"
    )
    Sys.sleep(0.5)
  }
  cli::cli_status_clear(id = sb)
  cli::cli_alert_success("Spreadsheet fully updated!")

  # Report how many pairings were checked and updated
  total <- nrow(all_pairs)
  invalid <- all_pairs %>% filter(status == "invalid") %>% nrow()
  old <- all_pairs %>% filter(status == "old") %>% nrow()
  unplayed <- all_pairs %>% filter(str_detect(status, "none")) %>% nrow()
  found <- all_pairs %>% filter(status == "found") %>% nrow()

  cli::cli_h2("Update summary")
  cli::cli_dl(c(
    "Total pairings" = total - invalid,
    "Previously recorded" = old,
    "Newly updated" = found,
    "Unplayed" = unplayed
    )
    )
  cli_alert_success("Update completed!")
  cli::rule(line = ":", right = paste0("Update completed: ",  as.character(Sys.time())))
}
