test_that("non numeric returns error", {
  expect_error(db_info("asd"))
  expect_error(db_info(list("a", 3, 4)))
})


test_that("API returns info and structure", {
  
  skip_if_offline(host = "r-project.org")
  
  sample_return <- db_info(7582) # Dame Street, Central Bank (Busy stop with 24hr bus services)
  
  expect_equal(names(sample_return), c("results", "stop"))
  expect_equal(names(sample_return$results),
               c("arrivaldatetime", "duetime", "departureduetime", "destination", "route", "monitored", "sourcetimestamp", "datatime", "stopnumber"))
  
  
})

test_that("bad urls gives error",{
  expect_error(db_info(7582, base_url = "asd"))
  
  expect_error(db_info(7582, api_path =  "/asd"), 
               "Unexpected http response 404")
  
})


