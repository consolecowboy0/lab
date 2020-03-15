test_that("cross_df works as expects", {
  gr <- goose_race(x = 5, y = 5)
  expect_equal(nrow(gr$locations), 25)
})
