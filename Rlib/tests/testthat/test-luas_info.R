test_that("luas api call works as expected", {
  
  skip_if_offline(host = "r-project.org")
  
  ranelagh <- 
    luas_info("ran")
  
  expect_identical(names(ranelagh), c("inbound", "outbound"))
  
  expect_error(luas_info("sdaran"), "failed to read")
})


test_that("check luas bus info works", {
  skip_if_offline(host = "r-project.org")
  
  stop_return <- luas_stop_info()
  
  expect_equal(
    names(stop_return),
    c("StopID", "Name", "Abbreviation", "LineID", "SortOrder", "IsEnabled", 
      "IsParkAndRide", "IsCycleAndRide", "ZoneCountA", "ZoneCountB", "Latitude", "Longitude", 
      "IrishName", "Line")
  )
  
  ranelagh_rowname <- as.integer(row.names(stop_return[which(stop_return$Name == "Ranelagh"),]))
  expect_equal(
    stop_return[which(stop_return$Name == 
                      "Ranelagh"), 
                c("StopID", "Name", "Abbreviation", "Latitude", "Longitude", "IrishName")],
    data.frame(StopID = 27L, Name = "Ranelagh", Abbreviation = "RAN", 
               Latitude = 53.326433333, Longitude = -6.2562027778, IrishName = "Raghnallach ", 
               stringsAsFactors = F, row.names = ranelagh_rowname)
  )
  
})