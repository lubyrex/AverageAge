
context("AverageAge")
required <<- 0

test_that("The average age is the correct number",{
  expect_equal(Average(), 50.17568-4.32e-06)
})

test_that("Maximum is the correct number",{
  expect_equal(Max(), 79)
})
test_that("Minimum is the correct number",{
  expect_equal(Min(),26)
})
test_that("Range is the correct number",{
  expect_equal(Range(),79-26)
})
test_that("Dataset is in the right form",{
  RequiredFunction()
  expect_output(actualData, " [1:370, 1]")
})
