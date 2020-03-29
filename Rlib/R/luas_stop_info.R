#' Luas Stop Info
#' 
#' Return information on Luas stops
#' 
#' @importFrom utils read.delim
#' @import dplyr
#' 
#' @export

luas_stop_info <- function(){
  
  stop_info <- read.delim("http://data.tii.ie/Datasets/Luas/StopLocations/luas-stops.txt", 
                          stringsAsFactors = F)
  
  stop_info %>%
    mutate(Line = recode(.data$LineID, `1` = "red", `2` = "green"))
  
}