test_that("viz_report_grid correct output", {
  # viz_report_grid expects dataframe
  expect_identical(viz_report_grid(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_report_grid(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_report_grid(c(1, 2, 3)), list(error = "Input not a dataframe"))
  
  # error should be the same as the one prepare_data gives
  expect_identical(viz_report_grid(prepare_data(c(1, 2, 3)))[[1]], 
                   "Input not a dataframe")
  expect_identical(viz_report_grid(prepare_data(patientdata[, -1]))[[1]], 
                   "Input not a dataframe")
  
  # correct output viz_report_grid is a svg string
  # expect_identical(class(viz_report_grid(prepare_data(patientdata))[[1]])[2], 
  #                  "svg")
})
