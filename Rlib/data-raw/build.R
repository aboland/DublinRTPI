
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

usethis::use_data(tidy_train_stations, overwrite = TRUE)


luas_stops <- luas_stop_info()

# Fix for non asscii warning in library check
luas_stops$Name <- gsub("á", intToUtf8(0x00E1), luas_stops$Name)

luas_stops$IrishName <- gsub("á", intToUtf8(0x00E1), luas_stops$IrishName)
luas_stops$IrishName <- gsub("é", intToUtf8(0x00E9), luas_stops$IrishName)
luas_stops$IrishName <- gsub("í", intToUtf8(0x00ED), luas_stops$IrishName)
luas_stops$IrishName <- gsub("ó", intToUtf8(0x00F3), luas_stops$IrishName)
luas_stops$IrishName <- gsub("ú", intToUtf8(0x00FA), luas_stops$IrishName)

luas_stops$IrishName <- gsub("Á", intToUtf8(0x00C1), luas_stops$IrishName)
luas_stops$IrishName <- gsub("É", intToUtf8(0x00C9), luas_stops$IrishName)
luas_stops$IrishName <- gsub("Ó", intToUtf8(0x00D3), luas_stops$IrishName)

# Not working so need to remove irish name....for now, need a fix
# luas_stops <- luas_stops[, -which(names(luas_stops) %in% "IrishName")]
# luas_stops$Name

usethis::use_data(luas_stops, overwrite = TRUE)
