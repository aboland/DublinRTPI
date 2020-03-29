#' API call for Luas real time info
#' 
#' @param stop_name Abbreviated stop name
#' 
#' @import xml2
#' @import dplyr
#' 
#' @export

luas_info <- function(stop_name){

  # # Call API
  # get_info <- 
  #   httr::GET(
  #   paste0("http://luasforecasts.rpa.ie/xml/get.ashx?action=forecast&stop=", stop_name,"&encrypt=false")
  #   ) ; get_info %>% content("text")
 
  get_info <- 
    tryCatch({
      list(
        data = 
          xml2::read_xml(
            paste0("http://luasforecasts.rpa.ie/xml/get.ashx?action=forecast&stop=", stop_name,"&encrypt=false")
          ), 
        error = NULL)
    }, 
    error = 
      function(e){
        return(list(data = NULL, error = "failed to read"))
    })
    
  if (!is.null(get_info$error))
    stop(get_info$error)
  # get_info %>% xml2::xml_name()
  # get_info %>% xml2::xml_children()
  # get_info %>% xml2::xml_text()
  # get_info %>% xml2::xml_structure()
  
  direction <- 
    get_info$data %>% 
    xml2::xml_find_all(".//direction") %>% 
    xml2::xml_attr("name")
  
  parsed_info <- 
    get_info$data %>% 
    xml2::xml_find_all(".//direction")
  
  inbound_info <- tryCatch({
    parsed_info[which(direction == "Inbound")] %>% 
      xml2::xml_find_all(".//tram") %>%
      xml2::xml_attrs(c("dueMins", "destination")) %>%
      do.call(rbind, .data)
  }, error = function(e) "Error parsing inbound info")
    
  
  outbound_info <- tryCatch({
    parsed_info[which(direction == "Outbound")] %>% 
      xml2::xml_find_all(".//tram") %>%
      xml2::xml_attrs(c("dueMins", "destination")) %>%
      do.call(rbind, .data)
  }, error = function(e) "Error parsing outbound info")
    
  return(list(inbound = inbound_info, outbound = outbound_info))
}

