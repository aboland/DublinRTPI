#' Real time info for multiple bus stops
#' 
#' This function uses the \link{db_scrape_stop_route_info} function to collect info for multiple stops.
#' 
#' @param stop_numbers A vector of bus stop numbers.
#' @return A data frame containing the times until the next buses at the selected stops.
#' @examples
#' \dontrun{db_scrape_multi_stop_info(c(334, 336))}
#' 
#' @import dplyr
#' @importFrom  rlang .data
#' 
#' @export
db_scrape_info <-
function(stop_numbers){
  
  stop_numbers = as.numeric(stop_numbers)
  if (sum(is.na(stop_numbers)) > 0)
    stop("Non numeric stop number!")
  
  stop_info <- list()
  combined_info <- NULL
    
  for (i in 1:length(stop_numbers)) {
    
    temp_info <- db_scrape_stop_route_info(stop_numbers[i])
    
    if (temp_info$errorcode == 0) {
      temp_info <- temp_info$results %>% 
        select(arrivaldatetime = .data$`Expected Time`, destination = .data$Destination, route = .data$Route) %>%
        mutate(datatime = Sys.time() + (60 * 60), stopnumber = stop_numbers[i],
               arrivaldatetime = case_when(.data$arrivaldatetime == "Due" ~ format(Sys.time() - (60 * 60), "%H:%M"),
                                           .data$arrivaldatetime != "Due" ~ .data$arrivaldatetime),
               arrivaldatetime = as.POSIXct(.data$arrivaldatetime, format = "%H:%M"),
               duetime = difftime(.data$arrivaldatetime, Sys.time() + (60 * 60), units = "mins") %>% round())
      
      if (length(which(temp_info$duetime <= 0)) > 0)
        temp_info$duetime[which(temp_info$duetime <= 0)] <- 0
      
      stop_info[[i]] <- temp_info
      combined_info <- bind_rows(combined_info, temp_info)
    }else{
      stop_info[[i]] <- temp_info$errormessage
    }

  }
  
  names(stop_info) <- paste0("number", stop_numbers)
  combined_info <- combined_info %>% arrange(.data$arrivaldatetime)
  return(list(results = combined_info, stop = stop_info))
}
