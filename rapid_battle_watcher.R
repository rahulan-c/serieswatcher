# RAPID BATTLE WATCHER
# Updated: 2021-09-04

# User choices ================================================================

# Enter sheet details
season_sheetid <- rb_sheetid
group_sheetnames <- rb_group_sheetnames
knockout_sheetnames <- rb_knockout_sheetnames

# Enter season dates
group_start_date <- rb_group_start_date
group_end_date <- rb_group_end_date
knockout_start_date <- rb_knockout_start_date
knockout_end_date <- rb_knockout_end_date

# Enter the current phase of the season ("group", "knockout")
current_phase <- rb_stage

# Enter the cell ranges that contain player pairings -- for group stages
pair_ranges <- rb_group_pair_ranges

# Enter cell ranges for knockout pairings
knockout_pair_ranges <- rb_knockout_pair_ranges

# googlesheets4 options
options(
  gargle_oauth_email = TRUE,
  googlesheets4_quiet = TRUE
)



## Search for a single group games between two players with known colours -----
# Used for group stage
FindGroupGame <- function(white, black,
                          from = LichessDateFormat(group_start_date, "00:00:01"),
                          to = LichessDateFormat(group_end_date, "23:59:59"),
                          perf = "rapid",
                          clock = 15,
                          increment = 10){


  # Query Lichess API
  query <- httr::GET(
    url = "https://lichess.org",
    path = paste0("/api/games/user/", white),
    query = list(
      since = from,
      until = to,
      perfType = perf,
      vs = black,
      rated  = "true",
      color = "white",
      clocks = "true"
    ),
    httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
    accept("application/x-ndjson")
  )

  # Stop on error
  if (query$status_code != 200) {
    print(http_status(query)$message)
    cli::cli_alert_danger("The Lichess API query returned an error. Stopping process.")
    break
  }

  # Convert response
  res <- query %>% httr::content("text",
                                 encoding = stringi::stri_enc_detect(httr::content(
                                   query, "raw"))[[1]][1, 1])

  # If games in the correct perf type are found, extract game data
  if (res != "") {
    res <- res %>%
      read_lines() %>%
      ndjson::flatten()

    # Only select games with the exact time control requested
    res <- res %>%
      filter(clock.initial == clock * 60 &
               clock.increment == increment)

  # If only one suitable game found...
  if (nrow(res) == 1) {

    score_w <- ifelse(res$status %in% c("draw", "stalemate"), 0.5, 99)
    if (score_w == 99) {
      score_w <- ifelse(res$winner == "white", 1, score_w)
      score_w <- ifelse(res$winner == "black", 0, score_w)
    }

    status <- "found"
    id <- res$id
    pts_w <- score_w
    pts_b <- 1 - score_w

    # If more than one suitable game found...
  } else if (nrow(res) > 1) {

    status <- "multiple"
    id <- paste0(str_c(res$id, collapse = " "))
    pts_w <- NA
    pts_b <- NA

    # If no suitable games in the TC are found...
  } else {
    status <- "unplayed"
    id <- NA
    pts_w <- NA
    pts_b <- NA
  }

    # If no suitable games in the perf type are found...
  } else {
    status <- "unplayed"
    id <- NA
    pts_w <- NA
    pts_b <- NA
  }

  return(list("status" = status,
              "id" = id,
              "pts_w" = pts_w,
              "pts_b" = pts_b))
}


## Search for all games in a match --------------------------------------------
# Used for knockout stage (and Quest)
# TODO

# Start watcher ---------------------------------------------------------------
# cli::rule(line = ">", right = paste0("Update started: ",  as.character(Sys.time())))
# cli::cli_h1("Rapid Battle Watcher")

# Update group games ----------------------------------------------------------
if(current_phase == "group") {
cli::cli_h2("Updating group games")

# Extract group players and pairings by looping over all divisions, groups,
# and colour lists
div_list <- list()
for (d in seq(1:length(pair_ranges))) {
  group_list <- list()
  for (g in seq(1:length(pair_ranges[[d]]))) {
    col_list <- list()
    for (c in seq(1:length(pair_ranges[[d]][[g]]))) {
      # Read player names
      players <- range_read(
        ss = season_sheetid,
        sheet = group_sheetnames[d],
        range = pair_ranges[[d]][[g]][c],
        col_names = F
      )
      Sys.sleep(1)
      col_list[[c]] <- players
    }

    # After identifying White and Black lists for a group
    # Combine to give pairing details, add cols for link, status, pts (2)
    col_list[[3]] <- bind_cols(col_list[[1]], col_list[[2]])

    # Convert result cols from lists to character format
    col_list[[3]] <- col_list[[3]] %>%
      mutate(...4 = sapply(...4, toString)) %>%
      mutate(...5 = sapply(...5, toString)) %>%
      mutate(status = rep(NA, nrow(col_list[[3]]))) %>%
      mutate(order = seq(1:nrow(.)))

    colnames(col_list[[3]])[1:5] <- c("white", "black", "link", "pts_w",
                                        "pts_b")

    # # Fix White and Black pts col formats
    col_list[[3]] <- col_list[[3]] %>%
      mutate(across(c(pts_w, pts_b), as.numeric))

    # cli::cli_alert_info("Identified {nrow(col_list[[3]])} group pairings to check")

    # Now see if each pairing has been played or not...
    # And if not, search for games...
    cli::cli_inform("Checking group pairings...")
    for (p in seq(1:nrow(col_list[[3]]))) {

      # cli::cli_inform("{p}/{nrow(col_list[[3]])} {col_list[[3]]$white[p]}-{col_list[[3]]$black[p]}")

      # Pairings with previously recorded gamelinks/results
      if (!(is.na(col_list[[3]]$link[p])))  {
        col_list[[3]]$status[p] <- "old"
        # cli::cli_inform("previously recorded")
        next
      }

      # Invalid pairings
      if ((is.na(col_list[[3]]$white[p])) || (col_list[[3]]$white[p] == "") ||
          (is.na(col_list[[3]]$black[p])) || (col_list[[3]]$black[p] == "")
      ) {
        col_list[[3]]$status[p] <- "invalid"
        # cli::cli_inform("invalid pairing")
        next
      } else {

        # cli::cli_inform("not played -> searching for games...")

      # Pairings that need to be checked...
      # Search for games - call find_games()
      search_res <- FindGroupGame(white = col_list[[3]]$white[p],
                               black = col_list[[3]]$black[p])

      # Report the pairing's new status (after searching)
      # cli::cli_inform("{search_res$status} {search_res$id}")
      Sys.sleep(2)

      # Update pairing data
      col_list[[3]]$status[p] <- search_res$status
      col_list[[3]]$link[p] <- ifelse(nchar(search_res$id) == 8,
                                      paste0("https://lichess.org/", search_res$id),
                                      ifelse(search_res$status == "multiple",
                                             "multiple games",
                                             ""))
      col_list[[3]]$pts_w[p] <- search_res$pts_w
      col_list[[3]]$pts_b[p] <- search_res$pts_b

      }

    }

    # Update the sheet after checking all group pairings
    googlesheets4::range_write(
      ss = season_sheetid,
      sheet = group_sheetnames[d],
      range = pair_ranges[[d]][[g]][c],
      data = col_list[[3]][,2:5],
      col_names = F,
      reformat = T
    )
    Sys.sleep(1)
    cli::cli_alert_success("Updated group game details in sheet")

    # Add to the list of group data
    group_list[[g]] <- col_list
  }

  # Add to the list of division data
  div_list[[d]] <- group_list
}
cli::cli_alert_success("Group phase data checked!")

}



# Update knockout games -------------------------------------------------------
if(current_phase == "knockout") {
  # cli::cli_h2("Updating knockout games")

  # TODO
  # cli::cli_alert_success("Knockout phase data checked!")

}

# Report total time taken
# tictoc::toc(log = T)

# End watcher -----------------------------------------------------------------
# cli::rule(line = "<", right = paste0("Update ended: ",  as.character(Sys.time())))
