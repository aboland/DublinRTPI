







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

# Function to access Dublin Bus RTPI API
db_get_multi_stop_info <- function(stop_numbers){
  
  # Check that the input stop numbers are numeric
  stop_numbers = as.numeric(stop_numbers)
  if(sum(is.na(stop_numbers))>0)
    stop("Non numeric stop number!")
  
  stop_info <- list()
  combined_info <- NULL
  
  # Progress function will only work in Shiny
  withProgress(message = 'Updating data', value = 0.1, {
    # Loop over all bus stops
    for(i in 1:length(stop_numbers)){
      # Update progress bar message
      incProgress(0, detail = paste0("Getting stop ", stop_numbers[i], " info"))
      
      # Call API
      temp_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_numbers[i],"&format=json"))
      
      if(temp_info$errorcode == 0){
        # If no error then tidy up data
        temp_info <- temp_info$results %>% 
          select(arrivaldatetime, duetime, departureduetime, destination, route, monitored, sourcetimestamp) %>%
          mutate(datatime = Sys.time(), stopnumber = stop_numbers[i])
        stop_info[[i]] <- temp_info
        combined_info <- bind_rows(combined_info, temp_info)
      }else{
        # If error return the error message
        stop_info[[i]] <- temp_info$errormessage
      }
      # increase progress bar indicator
      incProgress(1/length(stop_numbers))
    }
  })
  names(stop_info) <- paste0("number", stop_numbers)
  combined_info <- combined_info %>% arrange(arrivaldatetime)
  return(list(results = combined_info, stop = stop_info))
}



 # ------------------------------------
 #
 #          Scraping version
 #
 # ------------------------------------

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


db_scrape_multi_stop_info <- function(stop_numbers){
  
  stop_numbers = as.numeric(stop_numbers)
  if(sum(is.na(stop_numbers))>0)
    stop("Non numeric stop number!")
  
  stop_info <- list()
  combined_info <- NULL
  
  # Progress function will only work in Shiny
  withProgress(message = 'Updating data', value = 0.1, {
    
  for(i in 1:length(stop_numbers)){
    # Update progress bar message
    incProgress(0, detail = paste0("Getting stop ", stop_numbers[i], " info"))
    
    temp_info <- db_scrape_stop_route_info(stop_numbers[i])
    
    if(temp_info$errorcode == 0){
      temp_info <- temp_info$results %>% 
        select(arrivaldatetime = `Expected Time`, destination = Destination, route = Route) %>%
        mutate(datatime = Sys.time(), stopnumber = stop_numbers[i])
      
      temp_info$arrivaldatetime[temp_info$arrivaldatetime=="Due"] <- format(Sys.time(), "%H:%M")
      temp_info$arrivaldatetime <- as.POSIXct(temp_info$arrivaldatetime, format="%H:%M")
      
      temp_info <- temp_info %>%
        mutate(duetime = difftime(arrivaldatetime, Sys.time(), units = "mins") %>% round())
      
      if(length(which(temp_info$duetime <= 0)) > 0)
        temp_info$duetime[which(temp_info$duetime <= 0)] <- 0
      
      stop_info[[i]] <- temp_info
      combined_info <- bind_rows(combined_info, temp_info)
    }else{
      stop_info[[i]] <- temp_info$errormessage
    }
    # increase progress bar indicator
    incProgress(1/length(stop_numbers))
  }
  })
  
  names(stop_info) <- paste0("number", stop_numbers)
  combined_info <- combined_info %>% arrange(arrivaldatetime)
  return(list(results = combined_info, stop = stop_info))
}







# ------------------------------------
#
#          Dart
#
# ------------------------------------

dart_stop_info <- function(station_name){
  api_data <- xmlParse(paste0("http://api.irishrail.ie/realtime/realtime.asmx/getStationDataByCodeXML?StationCode=", station_name))
  stop_info <- xmlToDataFrame(api_data)
  
  stop_info <- stop_info %>% select(Destination, Status, Lastlocation, Duein, Exparrival, Expdepart, Direction, Traintype)
  return(stop_info)
}
