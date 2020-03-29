
# Save data
setwd("Rlib/")
library(XML)


raw_bus_stop_info <- 
  jsonlite::fromJSON(
    paste0("https://data.smartdublin.ie/cgi-bin/rtpi/busstopinformation?format=json")
    )

bus_stop_list <- raw_bus_stop_info$results
bus_stop_list[,c("stopid", "displaystopid", "shortname", "fullname", "shortnamelocalized", "fullnamelocalized")]

usethis::use_data(bus_stop_list, overwrite = TRUE)

raw_station_data <- 
  xmlParse("http://api.irishrail.ie/realtime/realtime.asmx/getAllStationsXML")
train_stations <- xmlToDataFrame(raw_station_data, stringsAsFactors = F)

usethis::use_data(train_stations, overwrite = TRUE)

tidy_train_stations <- as.list(gsub(" ", "", tolower(train_stations$StationCode)))
names(tidy_train_stations ) <- train_stations$StationDesc

usethis::use_data(tidy_train_stations , overwrite = TRUE)








