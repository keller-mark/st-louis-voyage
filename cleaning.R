library(stringr)

setwd("~/hackathons/datachallenge2018")

passengers <- read.csv("data/PassengerData.csv")
ship_journey <- read.csv("data/ShipJourney.csv")
events <- read.csv("data/EventTable.csv")

date.standardize <- function(raw_date) {
  date_string <- as.character(raw_date)
  matches <- str_match(date_string, "^([0-9])\\/\\s*\\/")
  print(matches)
}

sapply(events[,3], date.standardize)
