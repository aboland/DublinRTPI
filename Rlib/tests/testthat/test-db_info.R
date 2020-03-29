test_that("non numeric returns error", {
  expect_error(db_info("asd"))
  expect_error(db_info(list("a", 3, 4)))
})


test_that("API returns info and structure", {
  
  skip_if_offline(host = "r-project.org")
  
  # if (is.character(RCurl::getURL("www.google.com"))) {
  #   have_connection <- TRUE
  # } else {
  #   have_connection  <- FALSE
  # }
  
  # if (have_connection) {
    sample_return <- db_info(7582) # Dame Street, Central Bank (Busy stop with 24hr bus services)
    
    expect_equal(names(sample_return), c("results", "stop"))
    expect_equal(names(sample_return$results),
                 c("arrivaldatetime", "duetime", "departureduetime", "destination", "route", "monitored", "sourcetimestamp", "datatime", "stopnumber"))
  # }
  
  
})



    
    