#' Real time info for multiple bus stops (API)
#' 
#' This function uses the Irish transport API to retrieve the rela time information about bus stops.
#' 
#' @param stop_numbers A vector of bus stop numbers.
#' @return A data frame containing the times until the next buses at the selected stops.
#' @examples
#' \dontrun{db_get_multi_stop_info(c(334, 336))}
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @import dplyr
#' 
#' @export
db_info <-
  function(stop_numbers){
    
    # Check that the input stop numbers are numeric
    stop_numbers = suppressWarnings(as.numeric(stop_numbers))
    if (sum(is.na(stop_numbers)) > 0)
      stop("Non numeric stop number!")
    
    stop_info <- list()
    combined_info <- NULL
    
    # Loop over all bus stops
    for (i in 1:length(stop_numbers)) {
      
      # Call API
      get_info <- httr::GET(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_numbers[i],"&format=json"))
      
      if (get_info$status_code == 200) {
        temp_info <- jsonlite::fromJSON(httr::content(get_info, "text"))
        
        # temp_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_numbers[i],"&format=json"))
        
        if (temp_info$errorcode == 0) {
          # If no error then tidy up data
          temp_info <- temp_info$results %>% 
            select(.data$arrivaldatetime, .data$duetime, .data$departureduetime, .data$destination, .data$route, .data$monitored, .data$sourcetimestamp) %>%
            mutate(datatime = Sys.time() + (60 * 60), stopnumber = stop_numbers[i])
          stop_info[[i]] <- temp_info
          combined_info <- bind_rows(combined_info, temp_info)
        }else{
          # If error return the error message
          stop_info[[i]] <- temp_info$errormessage
        }
      }else{
        stop("Unexpected http response ", get_info$status_code)
      }
    }
    
    names(stop_info) <- paste0("number", stop_numbers)
    combined_info <- combined_info %>% arrange(.data$arrivaldatetime)
    return(list(results = combined_info, stop = stop_info))
  }
