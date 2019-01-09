

#
#  https://data.smartdublin.ie/dataset/real-time-passenger-information-rtpi-for-dublin-bus-bus-eireann-luas-and-irish-rail
#
#  http://api.irishrail.ie/realtime/index.htm?realtime_irishrail
#
#



library(jsonlite)
library(tidyverse)


my_stop <- jsonlite::fromJSON("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=334&format=json")
short_my_stop <- my_stop$results %>% select(duetime, departureduetime, destination, route, monitored, sourcetimestamp)
short_my_stop %>% filter(route == 140)


db_get_stop_route_info <- function(stop_number, selected_route = NULL){
  
  if(!is.null(selected_route)){
    all_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_number,"&routeid=",selected_route,"&format=json"))
  }else{
    all_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_number,"&format=json"))
  }
  
  if(all_info$errorcode == 0){
    tidy_info <- all_info$results %>% select(duetime, departureduetime, destination, route, monitored, sourcetimestamp)
  }else{
    tidy_info <- all_info$errormessage
  }
  return(tidy_info)
}

get_stop_info(334, 140)


get_stop_info(896)



