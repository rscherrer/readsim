## This file contains tests of the mock data writing function

# Simplest use case
test_that("Write data", {

  # Write a dummy file
  write_data(c(1, 2, 3, 4), "mock.dat")

  # Check
  expect_true(file.exists("mock.dat"))
  expect_true(file.info("mock.dat")$size == 32) # 4 * 8 bytes
  expect_true(file.info("mock.dat")$isdir == FALSE)

  # Read the file back in
  con <- file("mock.dat", "rb")
  values <- readBin(con, what = "numeric", n = 4)
  close(con)

  # Check
  expect_equal(values, c(1, 2, 3, 4))
  expect_equal(length(values), 4)
  expect_equal(typeof(values), "double")
  expect_equal(class(values), "numeric")

  # Delete the file
  file.remove("mock.dat")

})

# Test that error when no values given
test_that("Error when no values given", {

  # Check
  expect_error(
    write_data(numeric(0), "mock.dat"),
    "values cannot have length 0"
  )

})

# Test that error when values not numeric
test_that("Error when values not numeric", {

  # Check
  expect_error(
    write_data("foo", "mock.dat"),
    "values must be of class numeric"
  )

})

# Test that error when filename of the wrong size
test_that("Error when filename of the wrong size", {

  # Check
  expect_error(
    write_data(c(1, 2, 3, 4), c("foo", "bar")),
    "filename must have length 1"
  )

})

# Test that error when filename not character
test_that("Error when filename not character", {

  # Check
  expect_error(
    write_data(c(1, 2, 3, 4), 1),
    "filename must be of class character"
  )

})
