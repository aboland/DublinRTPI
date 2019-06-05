#' Get the real time info for a DART station
#' 
#' @param station_name A station name, short version.
#' @return A data frame containing the times until the next train at the selected station.
#' @examples
#' \dontrun{dart_stop_info("tara")}
#' 
#' @import XML
#' @import dplyr
#' @importFrom  rlang .data
#' 
#' @export
dart_info <-
function(station_name){
  api_data <- xmlParse(paste0("http://api.irishrail.ie/realtime/realtime.asmx/getStationDataByCodeXML?StationCode=", station_name))
  stop_info <- xmlToDataFrame(api_data)
  
  stop_info <- stop_info %>% select(.data$Destination, .data$Status, .data$Lastlocation, .data$Duein, .data$Exparrival, .data$Expdepart, .data$Direction, .data$Traintype)
  return(stop_info)
}
