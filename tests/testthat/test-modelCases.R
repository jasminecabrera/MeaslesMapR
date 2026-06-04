test_that("modelCases", {

# type
expect_s3_class(modelCases(), "gt_tbl")
expect_s3_class(modelCases(interaction = F), "gt_tbl")
expect_type(modelCases(), "list")
expect_type(modelCases(interaction = F), "list")

# error
expect_error(modelCases(interaction = error))
expect_error(modelCases(interaction = "error"))})
