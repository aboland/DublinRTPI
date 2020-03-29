test_that("non numeric returns error", {
  expect_error(db_scrape_info("asd"))
  expect_error(db_scrape_info(list("a", 3, 4)))
})



test_that("Test that data can be scraped", {
  
  skip_if_offline("www.google.com")
    
  sample_return <- db_scrape_info(7582) # Dame Street, Central Bank (Busy stop with 24hr bus services)
  
  expect_equal(names(sample_return), 
               c("results", "stop"))
  
})