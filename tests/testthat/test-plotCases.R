test_that("plotCases works", {
  # type
  expect_s3_class(plotCases(c("Americas")), "ggplot")
  expect_s3_class(plotCases("Western Pacific"), "ggplot")
  expect_s3_class(plotCases("Americas", legend = TRUE), "ggplot")

  # expect error
  expect_error(plotCases("apple"))
  expect_error(plotCases(c("Americas", "Africa")))})
