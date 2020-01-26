test_that("API returns info and structure", {
  
  if (is.character(RCurl::getURL("www.google.com"))) {
    have_connection <- TRUE
  } else {
    have_connection  <- FALSE
  }
  
  if (have_connection) {
    sample_return <- dart_info("tara")
    expect_equal(names(sample_return),
                 c("Destination", "Status", "Lastlocation", "Duein", "Exparrival", "Expdepart", "Direction", "Traintype"))
    
    expect_error(dart_info("not-a-station-name"))
  }
  
})