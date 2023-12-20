test_that("viz_carousel correct output", {
  # viz_carousel expects dataframe
  expect_identical(viz_carousel(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_carousel(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_carousel(c(1, 2, 3)), list(error = "Input not a dataframe"))
  
  # error should be the same as the one prepare_data gives
  expect_identical(viz_carousel(prepare_data(c(1, 2, 3)))[[1]],
                   "Input not a dataframe")
  expect_identical(viz_carousel(prepare_data(patientdata[, -1]))[[1]],
                   "Input not a dataframe")
  
  # correct output viz_carousel is a svg string
  # expect_identical(class(viz_carousel(prepare_data(patientdata))[[1]])[2],
  #                  "svg")
})

test_that("Correct input for viz_carousel returns an svg", {
  data <- opencpu_like_parse_json(gettestfilepath('json/answers.json'))
  result = viz_carousel(prepare_data(JSON_to_DF(data$answers)));
  
  # # Iterate over several slider images
  for( i in names(result$svgs) ) {
    expect_match(
      result$svgs[[i]],
      "<svg"
    )
  }

  if(Sys.getenv("CI_COMMIT_SHA") == '') {
    for( i in names(result$svgs) ) {
      filename <- paste0("../../svgs/", i , ".svg")
      writeLines(result$svgs[[i]], filename, useBytes = TRUE)
    }
  }
})
