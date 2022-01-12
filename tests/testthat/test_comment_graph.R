test_that("comment_graph correct output", {
  expect_error(comment_graph())
  expect_error(comment_graph(NULL),)
  expect_identical(comment_graph(NA), NA)
  expect_identical(comment_graph(""), "")
  # Tests whether strings are correctly split onto newlines
  string_small <- "test123"
  expect_identical(comment_graph(string_small), "test123")
  string_medium <- "test1234567891011121314151617181920212223"
  expect_identical(comment_graph(string_medium),
                   "test1234567891011121\n314151617181920212223")
  string_long <- "test123456789101112131415161718192021222324252627282930313233343536373839404142434445"
  expect_identical(comment_graph(string_long),
                   "test123456789101112131415161\n718192021222324252627282930313\n233343536373839404142434445")
})
