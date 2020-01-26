test_that("non numeric returns error", {
  expect_error(db_scrape_info("asd"))
  expect_error(db_scrape_info(list("a", 3, 4)))
})