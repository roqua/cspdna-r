test_that("viz_poster wrong input", {
  # viz_poster expects dataframe
  expect_identical(viz_poster(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_poster(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_poster(c(1, 2, 3)), list(error = "Input not a dataframe"))

  # error should be the same as the one prepare_data gives
  expect_identical(viz_poster(prepare_data(c(1, 2, 3)))[[1]],
                   "Input not a dataframe")
  expect_identical(viz_poster(prepare_data(patientdata[, -1]))[[1]],
                   "Input not a dataframe")
})

test_that("Correct input returns an svg", {
  data <- opencpu_like_parse_json(gettestfilepath('json/answers.json'))
  result = poster(data$answers);
  expect_match(
    result$svgs$poster,
    "<svg"
  )
  if(Sys.getenv("CI_COMMIT_SHA") == '') {
    writeLines(result$svgs$poster, "../../svgs/poster.svg", useBytes = TRUE)
  }
})
