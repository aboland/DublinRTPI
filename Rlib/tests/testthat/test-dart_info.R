test_that("Main function returns info and structure", {
  
  if (is.character(RCurl::getURL("www.google.com"))) {
    have_connection <- TRUE
  } else {
    have_connection  <- FALSE
  }
  
  if (have_connection) {
    sample_return <- 
      dart_info("tara")
    expect_equal(names(sample_return),
                 c("Destination", "Status", "Lastlocation", "Duein", "Exparrival", "Expdepart", "Direction", "Traintype"))
    
    expect_equal(dart_info("wbrok"),
                 data.frame(Destination = "", Status = "No Information", Lastlocation = "", Duein = "", Exparrival = "", Expdepart = "", Direction = "", Traintype = ""))
    expect_error(dart_info("not-a-station-name"))
    
    
    # sample_return2 <- 
    #   dart_api("tara")
    # expect_equal(names(sample_return2),
    #              c("Servertime", "Traincode", "Stationfullname", "Stationcode", "Querytime", "Traindate", "Origin", "Destination", 
    #                "Origintime", "Destinationtime", "Status", "Lastlocation", "Duein", "Late", "Exparrival", "Expdepart", "Scharrival", 
    #                "Schdepart", "Direction", "Traintype", "Locationtype"))
    
  }
})


test_that("API returns info and structure", {
  
  if (is.character(RCurl::getURL("www.google.com"))) {
    have_connection <- TRUE
  } else {
    have_connection  <- FALSE
  }
  
  if (have_connection) {
  sample_return2 <- 
    dart_api("tara")
  
  if (nrow(sample_return2) > 0)  # Stop faliure if test's run at nighttime
    expect_equal(names(sample_return2),
                 c("Servertime", "Traincode", "Stationfullname", "Stationcode", "Querytime", "Traindate", "Origin", "Destination", 
                   "Origintime", "Destinationtime", "Status", "Lastlocation", "Duein", "Late", "Exparrival", "Expdepart", "Scharrival", 
                   "Schdepart", "Direction", "Traintype", "Locationtype"))
  }
  
  expect_error(
    dart_api("nsasn", 
             paste0('"', "nsasn", '"', " is not a valid station! See 'tidy_station_list' for list of stations."))
  )
  
})