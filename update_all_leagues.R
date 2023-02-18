# UPDATE ALL ACTIVE COMMUNITY LEAGUES

# Sequentially checks for games played in Series, Rapid Battle and Quest before
# updating each league's current Google spreadsheet with relevant game/match
# data, including game URLs and results. For Series, when a recently played game
# is identified, it also sends a direct message on Lichess to both players,
# notifying them that their game has been picked up and recorded. The messages
# are sent from https://lichess.org/@/sheetle2.

# Calls a separate update script for each league:
# /series_watcher.R
# /rapid_battle_watcher.R
# /quest_watcher.R

# TODO (someday)
# - extend Rapid Battle script to cover the knockout phase


# Define active leagues / settings ============================================

## Active leagues -------------------------------------------------------------
series_active <- FALSE
rb_active <- FALSE

quest_active <- TRUE


## Series settings ------------------------------------------------------------
series_season_start <- "2022-12-05"
series_season_end <- "2023-02-19"
series_sheetid <- "1tJc10qM9FRIwZ4ygJpQwOEUzf4oKAZ-ZpNy8zfBDrqU"
series_sheetname <- "API"
series_pair_ranges <- c("B2:E25", "G2:J25", "L2:O25", "Q2:T25",
                        "V2:Y25", "AA2:AD25", "AF2:AI25")

## Rapid Battle settings ------------------------------------------------------
rb_stage <- "group" # "group", "knockout"
rb_sheetid <- "1UsO4lIhJz2ml0JdKXjnB5pBtLKnyDqsmrwPgijsDsAY"
rb_group_sheetnames <- c("Div A Group", "Div B Group")
rb_knockout_sheetnames <- c("Div A Playoffs", "Div B Playoffs")
rb_group_start_date <- "2022-09-05"
rb_group_end_date <- "2022-10-23"
rb_knockout_start_date <- "2022-10-24"
rb_knockout_end_date <- ""
rb_group_pair_ranges <- list(
  # Div A
  list(c("A11:A40", "D11:G40"),
       c("J11:J40", "M11:P40"),
       c("A51:A80", "D51:G80"),
       c("J51:J80", "M51:P80")),
  # Div B
  list(c("A11:A22", "D11:G22"),
       c("J11:J22", "M11:P22"),
       c("A33:A44", "D33:G44"),
       c("J33:J44", "M33:P44"))
)

rb_knockout_pair_ranges <- list()


## Infinite Quest settings ----------------------------------------------------
quest_sheetid <- "1Y_jYuHJUnDqfYMpO3YIainspTJa7o3hgd6smeiHfe20"
quest_sheetname <- "Results"


## Lichess API token ----------------------------------------------------------
root <- "C:/Users/rahul/Documents/Github/serieswatcher/"
token <- read.delim2(paste0(root, "api_token.txt"),
                     header = F)[1,1]



# Packages and functions ======================================================

# Packages
library(tidyverse)
library(googlesheets4)
library(httr)
library(lubridate)
library(jsonlite)
library(data.table)
library(stringi)
library(ndjson)
library(cli)
library(tictoc)
library(here)
library(glue)
library(emo)
library(prettyunits)

# Functions
## Format search dates correctly ----------------------------------------------
LichessDateFormat <- function(date, time_modifier){
  date <- as.numeric(formatC(
    as.numeric(lubridate::parse_date_time(paste0(date, " ", time_modifier), "ymdHMS")) * 1000,
    digits = 0, format = "f"
  ))
  return(date)
}


# Update active leagues =======================================================
tictoc::tic()
cli::cli_alert_info("Starting all league updates: {Sys.time()}")


## Update Series --------------------------------------------------------------

# Source SeriesUpdate function
source(paste0(root, "series_watcher.R"))

# If Series is active, check/update the relevant season spreadsheet
if(series_active) {
  cli::cli_alert_info("Series update started: {Sys.time()}")
  SeriesUpdate(
    season_start = series_season_start,
    season_end = series_season_end,
    season_sheetkey = series_sheetid,
    results_sheet = series_sheetname,
    pairing_ranges = series_pair_ranges,
    token = token
  )
  cli::cli_alert_success("Series update completed: {Sys.time()}")
}


## Update Rapid Battle --------------------------------------------------------

# If Rapid Battle is active, check/update the relevant stage data (group/knockout)
if(rb_active){
  if(series_active){Sys.sleep(60)}
  cli::cli_alert_info("Rapid Battle update started: {Sys.time()}")
  source(paste0(root, "rapid_battle_watcher.R"))
  cli::cli_alert_success("Rapid Battle update completed: {Sys.time()}")
}

## Update Infinite Quest ------------------------------------------------------

# If Infinite Quest is active, check/update the relevant spreadsheet
if(quest_active){
  if(rb_active){Sys.sleep(60)} else if(series_active){Sys.sleep(60)}
  cli::cli_alert_info("Infinite Quest update started: {Sys.time()}")
  source(paste0(root, "quest_watcher.R"))
  cli::cli_alert_success("Infinite Quest update completed: {Sys.time()}")
}

runtime <- tictoc::toc(log = F)

cli::cli_rule()
cli::cli_alert_info("Completed all league updates in {prettyunits::pretty_sec(runtime$toc - runtime$tic)}")
cli::cli_rule()


# ==== Run script every 6 hours using Windows Task Scheduler ==================
#
# Uses the {taskscheduleR} package
# Run this snippet in the Rstudio console
#
# taskscheduleR::taskscheduler_create(taskname = "check_community_leagues",
#                                     rscript = glue::glue("{here::here()}/update_all_leagues.R"),
#                                     schedule = "HOURLY",
#                                     starttime = "17:00",
#                                     modifier = 6)



