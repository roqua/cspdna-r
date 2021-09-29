test_that("time_count returns appropriate index", {
  expect_equal(time_count("2021-09-28 16:27:25"), 0)
  expect_equal(time_count("2021-09-28 16:27:25", "day"), 0)
  expect_equal(time_count(c("2021-09-28 16:27:25", "2021-10-28 16:27:25")),
                          c(0, 4))
  expect_equal(time_count(c("2021-09-28 16:27:25", "2021-10-28 16:27:25"),
                          "day"),
               c(0, 30))
})
