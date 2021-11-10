

library(tidyverse)
library(jsonlite)
library(httr)

api_key <-

url_json <- "https://public-api.tracker.gg/v2/splitgate/standard/matches/steam/dragongoku"

split_json <- jsonlite::fromJSON(url_json)
