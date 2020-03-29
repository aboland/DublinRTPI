#' API call for Luas real time info
#' 
#' @param stop_name Abbreviated stop name
#' @param base_url The base url for API endpoint
#' @param api_path Path for API endpoint
#' 
#' @import xml2
#' @import dplyr
#' 
#' @export

luas_info <- function(stop_name, base_url = "http://luasforecasts.rpa.ie", api_path = "/xml/get.ashx"){

  # # Call API
  # get_info <- 
  #   httr::GET(
  #   paste0("http://luasforecasts.rpa.ie/xml/get.ashx?action=forecast&stop=", stop_name,"&encrypt=false")
  #   ) ; get_info %>% content("text")
  
  # test <- 
  #   xmlParse(
  #     paste0("http://luasforecasts.rpa.ie/xml/get.ashx?action=forecast&stop=", "ran","&encrypt=false")
  #   ) %>% 
  #   xmlToList()
  
  
  
  get_info <- 
    tryCatch({
      list(
        data = 
          xml2::read_xml(
            paste0(base_url, api_path, "?action=forecast&stop=", stop_name,"&encrypt=false")
          ), 
        error = NULL)
    }, 
    error = 
      function(e){
        return(list(data = NULL, error = "failed to read"))
    })
    
  if (!is.null(get_info$error))
    stop(get_info$error)
  
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

