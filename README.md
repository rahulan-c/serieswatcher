# Series watcher

Series is an online chess league that's run within the [Lichess4545](https://www.lichess4545.com/) community with a classical time control of 90+30.

The code in this repository is used to periodically check for recently played games between players paired together in the current Series season. When it finds a 90+30 game that is likely to be a Series game, it updates the current Series spreadsheet - a Google Sheets file -  with the game's URL and result, before notifying both players that their game has been recorded through a Lichess message. 

## Requirements

- Read/write access to the current Series spreadsheet
- A Lichess API token with msg:write permissions: https://lichess.org/account/oauth/token

## Usage

For a one-off update, call the SeriesUpdate function:

```
source(series_watcher.R)

# Current Series spreadsheet ID
series_sheet <- "1VCA6XIJikVlakblodyfqXcy3rf7UkLPGJ5jlpi6CZ0E"

SeriesUpdate(
  season_start = "2021-08-09",  # use the format "YYYY-MM-DD"
  season_end = "2021-11-01",    # allowing for games to be played after the official end
  season_sheetkey = series_sheet,
  results_sheet = "API",        # sheet to be updated 
  pairing_ranges = c(
    "B2:E45", "G2:J45", "L2:O45", "Q2:T45",
    "V2:Y45", "AA2:AD45", "AF2:AI45"
  )
)
```

To automate the updates, I use [taskscheduleR](https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html) to create a new Windows Task Scheduler task that runs update_series.R every six hours:

```
myscript <- ".../update_series.R"
taskscheduleR::taskscheduler_create(taskname = "update_series", 
                                    rscript = myscript, 
                                    schedule = "HOURLY", 
                                    starttime = "00:05", 
                                    modifier = 6)
```

## Acknowledgements

Credit should go to [erv123](https://www.lichess4545.com/team4545/player/erv123/) for developing the first Series results update script. 



