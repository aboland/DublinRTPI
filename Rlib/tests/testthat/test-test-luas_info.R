test_that("luas api call works as expected", {
  
  skip_if_offline(host = "r-project.org")
  
  ranelagh <- 
    luas_info("ran")
  
  expect_identical(names(ranelagh), c("inbound", "outbound"))
  
  expect_error(luas_info("sdaran"), "failed to read")
})
