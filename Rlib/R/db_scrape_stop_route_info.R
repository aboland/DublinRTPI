#' Real time info for singlebus stops
#' 
#' This function uses the Dublin Bus website to retrieve the real time information about a single bus stop.
#' 
#' @param stop_number A numeric value for the bus stop number.
#' @return A data frame containing the times until the next buses at the selected stop.
#' @examples
#' \dontrun{db_scrape_stop_route_info(334)}
#' 
#' @import dplyr
#' @import stringr
#' @importFrom  rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom  rlang .data
#' 
db_scrape_stop_route_info <-
function(stop_number){
  
  webpage_data <- tryCatch({
    read_html(paste0("http://www.dublinbus.ie/RTPI/Sources-of-Real-Time-Information/?searchtype=view&searchquery=", stop_number))
  }, error = function(e) stop("Unable to reach site")
  )
  
  all_text <- 
    webpage_data  %>% 
    html_nodes('#real-time-display') %>% 
    html_text()
  
  if (length(all_text) > 0) {
    tidy_text <- 
      all_text %>% 
      str_split("\r\n") %>% 
      unlist() %>% 
      str_trim()
    
    start_text <- which(tidy_text == "Route")
    end_text <- which(tidy_text == "Accessible")
    
    if (length(start_text) > 0) {
      useful_info <- tidy_text[start_text:(end_text - 1)]
      useful_info <- useful_info[which(useful_info != "" & useful_info != "Notes")]
      useful_table <- matrix(useful_info[-(1:3)], ncol = 3, byrow = T, dimnames = list(NULL, useful_info[1:3]))
    
      return(list(results = as_tibble(useful_table), errorcode = 0))
    }else{
      return(list(errorcode = 1, errormessage = "No bus times available"))
    }
  }else{
    return(list(errorcode = 1, errormessage = "No results found"))
  }
}
