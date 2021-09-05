# Update Series spreadsheet
# Sources series_watcher.R and calls SeriesUpdate function

# Currently defined for Series S15

# Source SeriesUpdate function
source("C:/Users/rahul/Documents/Github/serieswatcher/series_watcher.R")

# Lichess API token - assumed to be saved in "api_token.txt"
# Needs to be loaded like this instead of from .Renviron to enable automation
token <- read.delim2("C:/Users/rahul/Documents/Github/serieswatcher/api_token.txt", header = F)[1,1]

# Call SeriesUpdate
SeriesUpdate(
  season_start = "2021-08-09",
  season_end = "2021-11-01",
  season_sheetkey = "1VCA6XIJikVlakblodyfqXcy3rf7UkLPGJ5jlpi6CZ0E",
  results_sheet = "API",
  pairing_ranges = c(
    "B2:E45", "G2:J45", "L2:O45", "Q2:T45",
    "V2:Y45", "AA2:AD45", "AF2:AI45"
  ),
  token = token
)
