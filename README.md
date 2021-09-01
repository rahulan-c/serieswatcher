# Series watcher

Series is an online chess league that's run within the [Lichess4545](https://www.lichess4545.com/) community with a classical time control of 90+30.

The code in this repository is used to periodically check for recently played games between players paired together in the current Series season. When it finds a Series game, it updates the current season spreadsheet (a Google Sheets file) with the game's URL and result. Both players are then notified through a direct message on Lichess.

## Requirements

- Read/write access to the current Series spreadsheet
- Lichess API token with `msg:write` permissions

## Usage

### One-off update 

Run update_series.R:

```
SeriesUpdate(
  season_start = "2021-08-09",
  season_end = "2021-11-01", # best to enter 1 day after the end date
  season_sheetkey = "1VCA6XIJikVlakblodyfqXcy3rf7UkLPGJ5jlpi6CZ0E",
  results_sheet = "API",
  pairing_ranges = c(
    "B2:E45", "G2:J45", "L2:O45", "Q2:T45",
    "V2:Y45", "AA2:AD45", "AF2:AI45"
  )
)
```

### Automated updates

I automate the updates with Windows Task Scheduler, using [taskscheduleR](https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html). No doubt there are other ways to do this too! The code below creates a new Task Scheduler task that runs update_series.R every six hours, starting from 00:05 (local time).

```
myscript <- "C:/Users/rahul/Documents/Github/serieswatcher/R/update_series.R"
taskscheduleR::taskscheduler_create(taskname = "update_series", 
                                    rscript = myscript, 
                                    schedule = "HOURLY", 
                                    starttime = "00:05", 
                                    modifier = 6)
```

## Acknowledgements

Credit should go to [erv123](https://www.lichess4545.com/team4545/player/erv123/) for developing the first Series results update script. 



