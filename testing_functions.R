

#
#  https://data.smartdublin.ie/dataset/real-time-passenger-information-rtpi-for-dublin-bus-bus-eireann-luas-and-irish-rail
#
#  http://api.irishrail.ie/realtime/index.htm?realtime_irishrail
#
#
#
# https://data.dublinked.ie/cgi-bin/rtpi/realtimebusinformation?stopid=184
# https://proxy.streamdata.io/https://data.dublinked.ie/cgi-bin/rtpi/realtimebusinformation?stopid=184
# http://dublinbus-api.heroku.com/stops

library(jsonlite)
library(tidyverse)

sample_bus <- jsonlite::fromJSON(paste0("http://dublinbus-api.heroku.com/stops/?stopid=", 334,"&format=json"))
jsonlite::fromJSON(paste0("http://dublinbus-api.heroku.com/stops?origin=53.343488,-6.249311&range=0.2&routes=2,3"))
jsonlite::fromJSON(paste0("http://dublinbus-api.heroku.com/services/2/0202"))
jsonlite::fromJSON("http://dublinbus-api.heroku.com/stops/westland+row/00495")

all_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", 334,"&routeid=",140,"&format=json"))


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





library(rvest)

as_data_frame(my_mat) %>%      # convert the matrix to a data frame
  gather(name, val, C_0:C_1) %>%  # convert the data frame from wide to long
  select(name, time, val) 



db_scrape_stop_route_info <- function(stop_number){
  
  test_webpage <- read_html(paste0("http://dublinbus.ie/en/RTPI/Sources-of-Real-Time-Information/?searchtype=view&searchquery=", stop_number))
  
  all_text <- test_webpage  %>% html_nodes('#real-time-display') %>% html_text()
  if(length(all_text)>0){
    tidy_text <- all_text %>% str_split("\r\n") %>% unlist() %>% str_trim()
    
    start_text <- which(tidy_text == "Route")
    end_text <- which(tidy_text=="Accessible")
    
    useful_info <- tidy_text[start_text:(end_text-1)]
    useful_info <- useful_info[which(useful_info!="" & useful_info != "Notes")]
    useful_table <- matrix(useful_info[-(1:3)], ncol=3, byrow = T, dimnames = list(NULL, useful_info[1:3]))
    return(list(results = as_data_frame(useful_table), errorcode = 0))
  }else{
    return(list(errorcode = 1, errormessage = "No results found"))
  }
}

db_scrape_stop_route_info(334)



db_scrape_multi_stop_info <- function(stop_numbers){
  
  stop_numbers = as.numeric(stop_numbers)
  if(sum(is.na(stop_numbers))>0)
    stop("Non numeric stop number!")
  
  stop_info <- list()
  combined_info <- NULL
  
  for(i in 1:length(stop_numbers)){
    temp_info <- db_scrape_stop_route_info(stop_numbers[i])
    
    if(temp_info$errorcode == 0){
      temp_info <- temp_info$results %>% 
        select(arrivaldatetime = `Expected Time`, Destination, Route) %>%
        mutate(datatime = Sys.time(), stopnumber = stop_numbers[i])
      
      temp_info$arrivaldatetime[temp_info$arrivaldatetime=="Due"] <- format(Sys.time(), "%H:%M")
      temp_info$arrivaldatetime <- as.POSIXct(temp_info$arrivaldatetime, format="%H:%M")
      
      temp_info <- temp_info %>%
        mutate(duetime = difftime(arrivaldatetime, Sys.time(), units = "mins") %>% round())
      
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


db_scrape_multi_stop_info(c(334, 335, 336))

sample_data2 <- db_scrape_multi_stop_info(c(334, 335, 336))

sample_data$results$arrivaldatetime
as.POSIXct(sample_data$results$arrivaldatetime, format="%H:%M")

sample_data2$results$arrivaldatetime

sample_data2$results$arrivaldatetime[sample_data2$results$arrivaldatetime=="Due"] <- format(Sys.time(), "%H:%M")
as.POSIXct(sample_data2$results$arrivaldatetime, format="%H:%M")



