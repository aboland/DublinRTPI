#' Get the real time info for a DART station
#' 
#' @param station_name A station name, short version.
#' @return A data frame containing the times until the next train at the selected station.
#' @examples
#' \dontrun{dart_stop_info("tara")}
#' 
#' @import dplyr
#' 
#' 
#' @export
dart_info <-
function(station_name){
  
  stop_info <- 
    dart_api(station_name)
  
  if (nrow(stop_info) < 1) {
    return(
      data.frame(Destination = "", Status = "No Information", Lastlocation = "", Duein = "", Exparrival = "", Expdepart = "", Direction = "", Traintype = ""))
    # stop("No info for ", station_name)
  }
  
  # stop_info <- 
  #   stop_info %>% 
  #   select(.data$Destination, .data$Status, .data$Lastlocation, .data$Duein, .data$Exparrival, .data$Expdepart, .data$Direction, .data$Traintype)
  return(stop_info[,c("Destination", "Status", "Lastlocation", "Duein", "Exparrival", "Expdepart", "Direction", "Traintype")])
}
