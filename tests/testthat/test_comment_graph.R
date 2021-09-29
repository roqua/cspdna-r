test_that("comment_graph correct output", {
  expect_error(comment_graph())
  expect_error(comment_graph(NULL),)
  expect_identical(comment_graph(NA), NA)
  expect_identical(comment_graph(""), "")
  # These tests are too narrow probably
  string_small <- "test123"
  expect_identical(comment_graph(string_small), "test123")
  # string_medium <- "test1234567891011121314151617181920212223"
  # expect_identical(comment_graph(string_medium),
  #                  "test12345678910111213\n14151617181920212223")
})
