library(stringr)
library(RJSONIO)

Sys.setlocale('LC_ALL','C')
setwd("~/hackathons/datachallenge2018")
geocode.api.key = Sys.getenv("GEOCODE_API_DATACHALLENGE_2018")

passengers <- read.csv("data/PassengerData.csv")
ship_journey <- read.csv("data/ShipJourney.csv")
events <- read.csv("data/EventTable.csv")

date.standardize <- function(raw_date) {
  date_string <- as.character(raw_date)
  matches_yr <- str_match(date_string, "^([0-9]+)\\/\\s*\\/\\s*$")
  matches_mn <- str_match(date_string, "^([0-9]+)\\/([0-9]+)\\/\\s*$")

  if(!is.na(matches_yr[1,2])) {
    return(paste(as.character(matches_yr[1,2]), "01", "01", sep="/"))
  } else if(!is.na(matches_mn[1,2]) && !is.na(matches_mn[1,3])) {
    return(paste(as.character(matches_mn[1,2]), as.character(matches_mn[1,3]), "01", sep="/"))
  } else {
    matches <- str_match(date_string, "^([0-9]+)\\/\\s*\\/\\s*$")
    return(date_string)
  }
}

coords.from.address <- function(event.place.cols) {
  if(event.place.cols[1,2] == "") {
    if(event.place.cols[1,1] == "") {
      # Country only
      address <- event.place.cols[1,3]
    } else {
      # City, country
      address <- paste(event.place.cols[1,1], event.place.cols[1,3], sep=", ")
    }
  } else {
    if(event.place.cols[1,1] == "") {
      # USA only
      address <- paste(event.place.cols[1,2], event.place.cols[1,3], sep=", ")
    } else {
      address <- paste(event.place.cols[1,1], event.place.cols[1,2], event.place.cols[1,3], sep=", ")
    }
  }
  clean_address <- iconv(address, to='ASCII//TRANSLIT')
  return(geocodeAddress(clean_address))
}

geocode.address <- function(address) {
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false&key=", geocode.api.key, sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  out
}

events[,"StartDate"] <- sapply(events[,"StartDate"], date.standardize)
events[,"EndDate"] <- sapply(events[,"EndDate"], date.standardize)
for(i in 1:nrow(events)) {
  events[i,c("Longitude", "Latitude")] <- coords.from.address(events[i,c("EventPlaceCurrent", "EventStateCurrent", "EventCountryCurrent")])
  
}
write.csv(events, "events_clean.csv")
