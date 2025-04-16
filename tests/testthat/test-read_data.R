## This file contains tests of the main data reading function

# Get directory of example simulation data
path <- system.file("extdata", "example", package = "readsim")

# Simplest use case with default arguments
test_that("Read one variable into a tibble", {

  # Read
  data <- read_data(path, variables = "time")

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 1)
  expect_equal(nrow(data), 6)
  expect_equal(colnames(data), "time")
  expect_equal(data$time[1], 0)

})

# Test with multiple variables with the same dimensions
test_that("Read two variables with same dimensions", {

  # Read
  data <- read_data(path, variables = c("time", "popsize"))

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 2)
  expect_equal(nrow(data), 6)
  expect_true(all(colnames(data) == c("time", "popsize")))
  expect_equal(data$time[1], 0)
  expect_equal(data$popsize[1], 10)

})

# Test with multiple variables and splitting
test_that("Read multiple variables and split some", {

  # Read
  data <- read_data(
    path,
    variables = c("time", "popsize", "patchsizes", "traitmeans"),
    split = c(1, 1, 10, 10)
  )

  # Note: in this example there are 10 patches

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 22)
  expect_equal(nrow(data), 6)
  expect_true(all(
    colnames(data) == c(
      "time", "popsize",
      paste0("patchsizes_", 1:10),
      paste0("traitmeans_", 1:10)
    )
  ))
  expect_equal(data$time[1], 0)

})

# Test with multiple variables and duplicating
test_that("Read multiple variables and duplicate some", {

  # Read
  data <- read_data(
    path,
    variables = c("time", "popsize", "patchsizes", "traitmeans"),
    dupl = c(10, 10, 1, 1)
  )

  # Note: the data should now be in a long format

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 4)
  expect_equal(nrow(data), 10 * 6)
  expect_true(all(
    colnames(data) == c("time", "popsize", "patchsizes", "traitmeans")
  ))
  expect_equal(data$time[1], 0)

})

# Test with both splitting and duplicating
test_that("Read multiple variables and split and duplicate", {

  # Read
  data <- read_data(
    path,
    variables = c("time", "popsize", "patchsizes", "traitmeans"),
    dupl = c(1, 1, 2, 2),
    split = c(1, 1, 20, 20)
  )

  # Note: this is an unusual combination but better safe than sorry

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 42)
  expect_equal(nrow(data), 6)
  expect_true(all(
    colnames(data) == c(
      "time", "popsize",
      paste0("patchsizes_", 1:20),
      paste0("traitmeans_", 1:20)
    )
  ))
  expect_equal(data$time[1], 0)

})

# Test with variable number of duplications
test_that("Read multiple variables and duplicate some with different lengths", {

  # Read
  data <- read_data(
    path,
    variables = c("time", "popsize"),
    dupl = list(seq_len(6), c(7, 7, 7, 0, 0, 0))
  )

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 2)
  expect_equal(nrow(data), 21)
  expect_true(all(
    colnames(data) == c("time", "popsize")
  ))
  expect_equal(data$time[1], 0)

})

# Test that error when directory is not character
test_that("Error when directory is not character", {

  # Check
  expect_error(
    read_data(5, variables = "time"),
    "dir must be of class character"
  )

})

# Test that error when directory is not length 1
test_that("Error when directory is not length 1", {

  # Check
  expect_error(
    read_data(c("a", "b"), variables = "time"),
    "dir must be of length 1"
  )

})

# Test that error when variables have length 0
test_that("Error when variables have length 0", {

  # Check
  expect_error(
    read_data(path, variables = c()),
    "variables cannot have length 0"
  )

})

# Test that error when variables are not character
test_that("Error when variables are not character", {

  # Check
  expect_error(
    read_data(path, variables = 5),
    "variables must be of class character"
  )

})

# Test that error when split has the wrong size
test_that("Error when split has the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", split = c()),
    "split must be of length 1 or the number of variables"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", split = c(1, 2)),
    "split must be of length 1 or the number of variables"
  )

})

# Test that error when split is not integer
test_that("Error when split is not integer", {

  # Check
  expect_error(
    read_data(path, variables = "time", split = "a"),
    "split must be coercible into class integer"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", split = 5.5),
    "split must be coercible into class integer"
  )

})

# Test that error when split has invalid values
test_that("Error when split has invalid values", {

  # Check
  expect_error(
    read_data(path, variables = "time", split = -1),
    "split must only have strictly positive values"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", split = 0),
    "split must only have strictly positive values"
  )

})

# Test that error when duplicate has the wrong size
test_that("Error when duplicate has the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", dupl = c()),
    "dupl must be of length 1 or the number of variables"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", dupl = c(1, 2)),
    "dupl must be of length 1 or the number of variables"
  )

})

# Test that error when duplicate is not integer
test_that("Error when duplicate is not integer", {

  # Check
  expect_error(
    read_data(path, variables = "time", dupl = "a"),
    "if not a list, dupl must be coercible into class integer"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", dupl = 5.5),
    "if not a list, dupl must be coercible into class integer"
  )

})

# Test that error when duplicate has invalid values
test_that("Error when duplicate has invalid values", {

  # Check
  expect_error(
    read_data(path, variables = "time", dupl = -1),
    "if not a list, dupl must only have strictly positive values"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", dupl = 0),
    "if not a list, dupl must only have strictly positive values"
  )

})

# Test that error when duplicate is a list but not of integers
test_that("Error when duplicate is a list of non-integer vectors", {

  # Check
  expect_error(
    read_data(path, variables = "time", dupl = list("a")),
    "dupl must contain vectors coercible into class integer"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", dupl = list(5.5)),
    "dupl must contain vectors coercible into class integer"
  )

})

# Test that error when duplicate is a list of vectors with invalid values
test_that("Error when duplicate is a list of vectors with invalid values", {

  # Check
  expect_error(
    read_data(path, variables = "time", dupl = list(-1)),
    "vector in dupl must only have positive values"
  )

  # This should work though
  data <- read_data(path, variables = "time", dupl = list(0))

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 1)
  expect_equal(nrow(data), 0)
  expect_equal(colnames(data), "time")

})

# Test that error when data file does not exist
test_that("Error when data file does not exist", {

  # Check
  expect_error(
    read_data(path, variables = "nonexistent"),
    paste("file", file.path(path, "nonexistent.dat"), "does not exist")
  )

})

# Test that error when duplicate when list duplicate has the wrong size
test_that("Error when duplicate has the wrong size for a specific variable", {

  # Check
  expect_error(
    read_data(path, variables = "time", dupl = list(c(1, 2, 3))),
    "wrong length of dupl for variable time"
  )

  # Note: here we only pass duplication rules for three values of variable
  # "time", while there are six values in total in the data file.

})

# Test when a data file is empty
test_that("Read when data file is empty", {

  # Read
  data <- read_data(path, variables = "empty")

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 1)
  expect_equal(nrow(data), 0)
  expect_equal(colnames(data), "empty")

})

# Test that error when split does not divide the number of values
test_that("Error when split does not divide the number of values", {

  # Check
  expect_error(
    read_data(path, variables = "time", split = 5),
    paste(
      "split must divide the number of (duplicated) values",
      "for variable time (length 6)"
    ),
    fixed = TRUE # when parentheses in error message
  )

  # Note: cannot split time into 5 columns since there are 6 values.

})

# Test with non-default name separator
test_that("Read with non-default name separator", {

  # Read
  data <- read_data(
    path,
    variables = c("time"),
    split = 2,
    sep = "."
  )

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 2)
  expect_true(all(
    colnames(data) == c("time.1", "time.2")
  ))

})

# Test error when number of rows is not the same across variables
test_that("Error when number of rows is not the same across variables", {

  # Check
  expect_error(
    read_data(path, variables = c("time", "patchsizes")),
    "number of rows is not the same across variables"
  )

})

# Test that tibble conversion works
test_that("Tibble conversion", {

  # Read
  data1 <- read_data(path, variables = c("time", "popsize"), is_tbl = TRUE)

  # Check
  expect_true(is.data.frame(data1))
  expect_true(tibble::is_tibble(data1))

  # Read
  data2 <- read_data(path, variables = c("time", "popsize"), is_tbl = FALSE)

  # Check
  expect_true(is.data.frame(data2))
  expect_false(tibble::is_tibble(data2))

})

# Test with non-default size (integer)
test_that("Read integers", {

  # Read
  data <- read_data(path, "integers", type = "integer", size = 4)

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 1)
  expect_equal(nrow(data), 3)
  expect_true(colnames(data) == "integers")
  expect_equal(data$integers[1], 1)
  expect_equal(data$integers[2], 2)
  expect_equal(data$integers[3], 3)

})

# Test with non-default size (logical)
test_that("Read logical", {

  # Read
  data <- read_data(path, "integers", type = "logical", size = 1)

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 1)
  expect_equal(nrow(data), 12)
  expect_equal(colnames(data), "integers")
  expect_equal(data$integers[1], TRUE)
  expect_equal(data$integers[2], FALSE)
  expect_equal(data$integers[3], FALSE)
  expect_equal(data$integers[4], FALSE)

})

# Test that error when type is of the wrong size
test_that("Error when type is of the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", type = c("integer", "double")),
    "type must be of length 1"
  )

})

# Test that error when type is not character
test_that("Error when type is not character", {

  # Check
  expect_error(
    read_data(path, variables = "time", type = 5),
    "type must be of class character"
  )

})

# Test that error when type is not one of the accepted types
test_that("Error when type is not one of the accepted types", {

  # Check
  expect_error(
    read_data(path, variables = "time", type = "foo"),
    "type must be one of numeric, integer, or logical"
  )

})

# Test that error when size is of the wrong size
test_that("Error when size is of the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", size = c(1, 2)),
    "size must be of length 1"
  )

})

# Test that error when size is not integer
test_that("Error when size is not integer", {

  # Check
  expect_error(
    read_data(path, variables = "time", size = "a"),
    "size must be coercible into class integer"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", size = 5.5),
    "size must be coercible into class integer"
  )

})

# Test that error when size has invalid value
test_that("Error when size has invalid value", {

  # Check
  expect_error(
    read_data(path, variables = "time", size = -1),
    "size must be strictly positive"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", size = 0),
    "size must be strictly positive"
  )

})

# Test that error when size does not divide file size
test_that("Error when size does not divide file size", {

  # Check
  expect_error(
    read_data(path, variables = "integers", size = 5),
    paste("size must divide the size of file", file.path(path, "integers.dat"))
  )

})

# Test that error when name separator of wrong size
test_that("Error when name separator is of the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", sep = c("_", "_")),
    "sep must be of length 1"
  )

})

# Test that error when name separator is not character
test_that("Error when name separator is not character", {

  # Check
  expect_error(
    read_data(path, variables = "time", sep = 1),
    "sep must be of class character"
  )

})

# Test that error when is_tbl is of the wrong size
test_that("Error when is_tbl is of the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", is_tbl = c(TRUE, FALSE)),
    "is_tbl must be of length 1"
  )

})

# Test that error when is_tbl is not logical
test_that("Error when is_tbl is not logical", {

  # Check
  expect_error(
    read_data(path, variables = "time", is_tbl = 1),
    "is_tbl must be of class logical"
  )

})

# Test that error when extension is of the wrong size
test_that("Error when extension is of the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", ext = c(".dat", ".txt")),
    "ext must be of length 1 or the number of variables"
  )

})

# Test that error when extension is not character
test_that("Error when extension is not character", {

  # Check
  expect_error(
    read_data(path, variables = "time", ext = 1),
    "ext must be of class character"
  )

})

# Test with another extension
test_that("Read with another extension", {

  # Read
  data <- read_data(path, variables = "otherext", ext = ".bin")

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 1)
  expect_equal(nrow(data), 6)
  expect_equal(colnames(data), "otherext")
  expect_equal(data$otherext[1], 0)

})

# Test with different extensions
test_that("Read with different extensions", {

  # Read
  data <- read_data(
    path,
    variables = c("time", "otherext"),
    ext = c(".dat", ".bin")
  )

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 2)
  expect_equal(nrow(data), 6)
  expect_true(all(colnames(data) == c("time", "otherext")))
  expect_equal(data$time[1], 0)
  expect_equal(data$otherext[1], 0)

})

# Test that it works when filling by column
test_that("Read when filling by column", {

  # Read
  data <- read_data(path, "patchsizes", split = 10, byrow = FALSE)

  # Read in long format
  patchsizes <- read_data(path, "patchsizes")[["patchsizes"]]

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 10)
  expect_equal(nrow(data), 6)
  expect_equal(data$patchsizes_1[1], patchsizes[1])
  expect_equal(data$patchsizes_1[2], patchsizes[2])
  expect_equal(data$patchsizes_1[3], patchsizes[3])
  expect_equal(data$patchsizes_1[4], patchsizes[4])

})

# Test that it works when filling by row
test_that("Read when filling by row", {

  # Read
  data <- read_data(path, "patchsizes", split = 10, byrow = TRUE)

  # Read in long format
  patchsizes <- read_data(path, "patchsizes")[["patchsizes"]]

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 10)
  expect_equal(nrow(data), 6)
  expect_equal(data$patchsizes_1[1], patchsizes[1])
  expect_equal(data$patchsizes_2[1], patchsizes[2])
  expect_equal(data$patchsizes_3[1], patchsizes[3])
  expect_equal(data$patchsizes_4[1], patchsizes[4])

})

# Test that error when byrow is of the wrong size
test_that("Error when byrow is of the wrong size", {

  # Check
  expect_error(
    read_data(path, variables = "time", byrow = c(TRUE, FALSE)),
    "byrow must be of length 1 or the number of variables"
  )

  # Another check
  expect_error(
    read_data(path, variables = "time", byrow = c()),
    "byrow must be of length 1 or the number of variables"
  )

})

# Test that error when byrow is not logical
test_that("Error when byrow is not logical", {

  # Check
  expect_error(
    read_data(path, variables = "time", byrow = 1),
    "byrow must be of class logical"
  )

})

# Test that it works with different filling types
test_that("Read with different filling types", {

  # Read
  data <- read_data(
    path,
    variables = c("patchsizes", "traitmeans"),
    split = c(10, 10),
    byrow = c(TRUE, FALSE)
  )

  # Read in long format
  patchsizes <- read_data(path, "patchsizes")[["patchsizes"]]
  traitmeans <- read_data(path, "traitmeans")[["traitmeans"]]

  # Check
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 20)
  expect_equal(nrow(data), 6)
  expect_equal(data$patchsizes_1[1], patchsizes[1])
  expect_equal(data$patchsizes_2[1], patchsizes[2])
  expect_equal(data$patchsizes_3[1], patchsizes[3])
  expect_equal(data$patchsizes_4[1], patchsizes[4])
  expect_equal(data$traitmeans_1[1], traitmeans[1])
  expect_equal(data$traitmeans_1[2], traitmeans[2])
  expect_equal(data$traitmeans_1[3], traitmeans[3])
  expect_equal(data$traitmeans_1[4], traitmeans[4])

})
