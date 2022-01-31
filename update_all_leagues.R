# UPDATE ALL ACTIVE COMMUNITY LEAGUES

# Sequentially checks for games played in Series, Rapid Battle and Quest before
# updating each league's current Google spreadsheet with relevant game/match
# data, including game URLs and results. For Series, when a recently played game
# is identified, it also sends a direct message on Lichess to both players,
# notifying them that their game has been picked up and recorded. The messages
# are sent from https://lichess.org/@/sheetle.

# Calls a separate update script for each league:
# /series_watcher.R
# /rapid_battle_watcher.R
# /quest_watcher.R

# TODO
# - extend Rapid Battle script to cover the knockout phase


# Define active leagues / settings ============================================

## Active leagues -------------------------------------------------------------
series_active <- FALSE
rb_active <- FALSE
quest_active <- TRUE


## Series settings ------------------------------------------------------------
series_season_start <- "2021-11-15"
series_season_end <- "2022-01-30"
series_sheetid <- "1WjKzwzWBElLlMimHdLO1tsbk9dxhXuCk_kD96OoBHpQ"
series_sheetname <- "API"
series_pair_ranges <- c("B2:E37", "G2:J37", "L2:O37", "Q2:T37",
                        "V2:Y37", "AA2:AD37", "AF2:AI37")

## Rapid Battle settings ------------------------------------------------------
rb_stage <- "group" # "group", "knockout"
rb_sheetid <- "1y2cQTKyMN01nLCRDXbv7Rq8LZ1c-xvarO335ruhuYpE"
rb_group_sheetnames <- c("Div A Group", "Div B Group")
rb_knockout_sheetnames <- c("Div A Playoffs", "Div B Playoffs")
rb_group_start_date <- "2021-11-10"
rb_group_end_date <- "2021-12-28"
rb_knockout_start_date <- "2021-06-15"
rb_knockout_end_date <- ""
rb_group_pair_ranges <- list(
  # Div A
  list(c("A10:A29", "D10:G29"),
       c("J10:J29", "M10:P29"),
       c("A39:A58", "D39:G58"),
       c("J40:J69", "M40:P69")),
  # Div B
  list(c("A10:A29", "D10:G29"),
       c("J10:J29", "M10:P29"),
       c("A39:A58", "D39:G58"),
       c("J39:J58", "M39:P58"))
)

rb_knockout_pair_ranges <- list()


## Infinite Quest settings ----------------------------------------------------
quest_sheetid <- "1Y_jYuHJUnDqfYMpO3YIainspTJa7o3hgd6smeiHfe20"
quest_sheetname <- "Raw Data"


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


# # Automate using Windows Task Scheduler =====================================
# Would need to be run in the console anyway
# Creates a task that runs this script every 6 hours
# taskscheduleR::taskscheduler_create(taskname = "check_community_leagues",
#                                     rscript = glue::glue("{here::here()}/update_all_leagues.R"),
#                                     schedule = "HOURLY",
#                                     starttime = "17:00",
#                                     modifier = 6)



