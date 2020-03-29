#' Call API for real time info for a DART station
#' 
#' @param station_name A station name, short version.
#' @param base_url The base url for API endpoint
#' @param api_path Path for API endpoint
#' 
#' @return A data frame containing the times until the next train at the selected station.
#' 
#' @examples
#' \dontrun{dart_api("tara")}
#' 
#' @import XML
#' 
#' @export
dart_api <-
  function(station_name, base_url = "http://api.irishrail.ie", api_path = "/realtime/realtime.asmx/getStationDataByCodeXML"){
    
    if (!station_name %in% gsub(" ", "", unlist(dublinRTPI::tidy_train_stations)))
      stop('"', station_name, '"', " is not a valid station! See 'train_station_list' for list of stations.")
    
    
    stop_info <- 
      tryCatch({
        api_data <- 
          xmlParse(
            paste0(base_url, api_path, "?StationCode=", station_name))
        xmlToDataFrame(api_data)
      }, 
      error = function(e){
        e$message <- paste("While reading API:", e, sep = " ")
        stop(e)
      })
    
    return(stop_info)
  }