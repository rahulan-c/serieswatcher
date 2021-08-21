# Series watcher

Series is an online chess league run within the [Lichess4545](https://www.lichess4545.com/) community for true aficionados of classical chess, in that the time control is 90+30 (!).

This code in this repository supports league moderation by regularly updating the current Series spreadsheet (a Google Sheets file) with the results and links of all recently completed games. 

This has been achieved by setting up a Github Action that runs the function *update_series()* at regular intervals (defined in .github/actions/schedule.yaml). 

When the details of a game are added to the sheet, both players are also notified on Lichess with a direct message.

## Requirements

- The package dependencies: dplyr, tibble, googlesheets4, httr, lubridate, jsonlite, data.table, stringi, ndjson
- Read/write access to the current Series spreadsheet
- A valid Lichess API token with the msg:write scope

## How to run

*If everything works as it should, no one else needs to use this package. It's only in a public repository to enable*

## Acknowledgements

I wouldn't have done this if I hadn't come across https://blog--simonpcouch.netlify.app/blog/r-github-actions-commit/. 

I originally wrote this script in Python for Series S14 (May 2021), and I've only redone it in R because this link told me it was possible to automate an R script by using Github Actions. Let's see...

Also, credit should go to Lichess4545 member **erv123** for developing the original Series update script. 



