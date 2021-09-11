# UPDATE ALL LEAGUES
# Unifies the update processes/scripts for Series, Rapid Battle and Quest

# Last updated: 2021-09-11


# Define active leagues / settings ============================================

## Active leagues -------------------------------------------------------------
series_active <- FALSE
rb_active <- TRUE
quest_active <- FALSE


## Series settings ------------------------------------------------------------
series_season_start <- "2021-08-09"
series_season_end <- "2021-11-01"
series_sheetid <- "1VCA6XIJikVlakblodyfqXcy3rf7UkLPGJ5jlpi6CZ0E"
series_sheetname <- "API"
series_pair_ranges <- c("B2:E45", "G2:J45", "L2:O45", "Q2:T45",
                        "V2:Y45", "AA2:AD45", "AF2:AI45")

## Rapid Battle settings ------------------------------------------------------
rb_stage <- "group" # "group", "knockout"
rb_sheetid <- "1N2ixyY6r_COHhBoFrmAzBaMu3taTlEQFIEhC6wJhgSI"
rb_group_sheetnames <- c("Section A Group Stage", "Section B Group Stage")
rb_knockout_sheetnames <- c("Div A Playoffs", "Div B Playoffs")
rb_group_start_date <- "2021-08-10"
rb_group_end_date <- "2021-09-20"
rb_knockout_start_date <- "2021-06-15"
rb_knockout_end_date <- ""
rb_group_pair_ranges <- list(
  # Div A
  list(c("A12:A53", "D12:G53"),
       c("J12:J53", "M12:P53"),
       c("A65:A106", "D65:G106"),
       c("J65:J106", "M65:P106")),
  # Div B
  list(c("A12:A53", "D12:G53"),
       c("J12:J53", "M12:P53"),
       c("A65:A106", "D65:G106"),
       c("J65:J106", "M65:P106"))
)

rb_knockout_pair_ranges <- list()


## Infinite Quest settings ----------------------------------------------------
quest_sheetid <- "1Y_jYuHJUnDqfYMpO3YIainspTJa7o3hgd6smeiHfe20"
quest_sheetname <- "Raw Data"



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
# source("C:/Users/rahul/Documents/Github/serieswatcher/series_watcher.R")
source(glue::glue("{here::here()}/series_watcher.R"))


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
  source(glue::glue("{here::here()}/rapid_battle_watcher.R"))
  cli::cli_alert_success("Rapid Battle update completed: {Sys.time()}")
}

## Update Infinite Quest ------------------------------------------------------

# If Infinite Quest is active, check/update the relevant spreadsheet
if(quest_active){
  if(rb_active){Sys.sleep(60)} else if(series_active){Sys.sleep(60)}
  cli::cli_alert_info("Infinite Quest update started: {Sys.time()}")
  source(glue::glue("{here::here()}/quest_watcher.R"))
  cli::cli_alert_success("Infinite Quest update completed: {Sys.time()}")
}

runtime <- tictoc::toc(log = F)

cli::cli_rule()
cli::cli_alert_info("Completed all league updates in {prettyunits::pretty_sec(runtime$toc - runtime$tic)}")
cli::cli_rule()


# # Automate using Windows Task Scheduler =====================================
# # Should be run in the console
# taskscheduleR::taskscheduler_create(taskname = "check_community_leagues",
#                                     rscript = glue::glue("{here::here()}/update_all_leagues.R"),
#                                     schedule = "HOURLY",
#                                     starttime = "23:00",
#                                     modifier = 6)



