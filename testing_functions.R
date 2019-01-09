

#
#  https://data.smartdublin.ie/dataset/real-time-passenger-information-rtpi-for-dublin-bus-bus-eireann-luas-and-irish-rail
#
#  http://api.irishrail.ie/realtime/index.htm?realtime_irishrail
#
#
#
# https://data.dublinked.ie/cgi-bin/rtpi/realtimebusinformation?stopid=184
# https://proxy.streamdata.io/https://data.dublinked.ie/cgi-bin/rtpi/realtimebusinformation?stopid=184


library(jsonlite)
library(tidyverse)


db_get_stop_route_info <- function(stop_number, selected_route = NULL){
  
  if(!is.null(selected_route)){
    all_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_number,"&routeid=",selected_route,"&format=json"))
  }else{
    all_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_number,"&format=json"))
  }
  
  if(all_info$errorcode == 0){
    tidy_info <- all_info$results %>% select(arrivaldatetime, duetime, departureduetime, destination, route, monitored, sourcetimestamp, monitored)
  }else{
    tidy_info <- all_info$errormessage
  }
  return(tidy_info)
}

db_get_stop_route_info(334, 140)


db_get_stop_route_info(896)


db_get_multi_stop_info <- function(stop_numbers){
  
  stop_numbers = as.numeric(stop_numbers)
  if(sum(is.na(stop_numbers))>0)
    stop("Non numeric stop number!")
  
  stop_info <- list()
  combined_info <- NULL
  
  for(i in 1:length(stop_numbers)){
    temp_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_numbers[i],"&format=json"))
    
    if(temp_info$errorcode == 0){
      temp_info <- temp_info$results %>% 
        select(arrivaldatetime, duetime, departureduetime, destination, route, monitored, sourcetimestamp, monitored) %>%
        mutate(datatime = Sys.time(), stopnumber = stop_numbers[i])
      stop_info[[i]] <- temp_info
      combined_info <- bind_rows(combined_info, temp_info)
    }else{
      stop_info[[i]] <- temp_info$errormessage
    }
    
  }
  names(stop_info) <- paste0("number", stop_numbers)
  combined_info <- combined_info %>% arrange(arrivaldatetime)
  return(list(results = combined_info, stop = stop_info))
}


db_get_multi_stop_info(c(334, 335, 336))



# http://api.irishrail.ie/realtime/realtime.asmx/getStationsFilterXML?StationText=ta
# http://api.irishrail.ie/realtime/realtime.asmx/getStationDataByCodeXML?StationCode=mhide 

library(XML)
library(dplyr)

sample_api_return <- xmlParse("http://api.irishrail.ie/realtime/realtime.asmx/getStationDataByCodeXML?StationCode=tara")
sample_data <- xmlToDataFrame(sample_api_return)

dart_stop_info <- function(station_name){
  api_data <- xmlParse(paste0("http://api.irishrail.ie/realtime/realtime.asmx/getStationDataByCodeXML?StationCode=", station_name))
  stop_info <- xmlToDataFrame(api_data)
  
  stop_info <- stop_info %>% select(Destination, Status, Lastlocation, Duein, Exparrival, Expdepart, Direction, Traintype)
  return(stop_info)
}


dart_stop_info("tara")

