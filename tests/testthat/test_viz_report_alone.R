test_that("viz_report_alone correct output", {
  # viz_report_alone expects dataframe
  expect_identical(viz_report_alone(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_report_alone(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_report_alone(c(1, 2, 3)), list(error = "Input not a dataframe"))
  
  # error should be the same as the one prepare_data gives
  expect_identical(viz_report_alone(prepare_data(c(1, 2, 3)))[[1]], 
                   "Input not a dataframe")
  expect_identical(viz_report_alone(prepare_data(patientdata[, -1]))[[1]], 
                   "Input not a dataframe")
  
  # correct output viz_report_alone is a svg string
  # expect_identical(class(viz_report_alone(prepare_data(patientdata))[[1]])[2], 
  #                  "svg")
 })

test_that("Correct input for viz_report_alone returns an svg", {
  data <- opencpu_like_parse_json(gettestfilepath('json/answers.json'))
  result = viz_report_alone(prepare_data(JSON_to_DF(data$answers)));
  expect_match(
    result$svgs$alone,
    "<svg"
  )
  if(Sys.getenv("CI_COMMIT_SHA") == '') {
    writeLines(result$svgs$alone, "../../svgs/alone.svg", useBytes = TRUE)
  }
})
