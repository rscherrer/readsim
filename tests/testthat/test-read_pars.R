## This file contains tests of the parameter reading function

# Get directory of example simulation data
path <- system.file("extdata", "example", package = "readsim")

# Simplest use case
test_that("Read parameters", {

  # Read
  pars <- read_pars(path)

  # Check
  expect_true(is.list(pars))
  expect_equal(length(pars), 26)
  expect_true(all(c(
    "type", "popsize", "pgood", "maxgrowths", "zwidths",
    "capacities", "stress", "steep", "dispersal", "mutation",
    "nchrom", "effect", "xmax", "ymax", "tradeoff",
    "selfing", "recombination", "tend", "tsave",
    "loadarch", "savepars", "savelog", "savearch",
    "talkative", "choose"
  ) %in% names(pars)))
  expect_equal(pars[["type"]], 1)
  expect_equal(pars[["popsize"]], 10)
  expect_true(all(pars[["pgood"]] == c(5.0, 0.8, 0.6, 0.5, 0.3, 0.1)))
  expect_true(all(pars[["maxgrowths"]] == c(2, 3)))
  expect_true(all(pars[["zwidths"]] == c(1, 2)))
  expect_true(all(pars[["capacities"]] == c(10000, 100)))
  expect_true(all(pars[["stress"]] == c(4, 0)))
  expect_equal(pars[["steep"]], 2)
  expect_equal(pars[["dispersal"]], 0.1)
  expect_equal(pars[["mutation"]], 1e-04)
  expect_equal(pars[["nchrom"]], 1)
  expect_equal(pars[["effect"]], 0.1)
  expect_equal(pars[["xmax"]], 5)
  expect_equal(pars[["ymax"]], 5)
  expect_equal(pars[["tradeoff"]], 0.005)
  expect_equal(pars[["selfing"]], 0.95)
  expect_equal(pars[["recombination"]], 1)
  expect_equal(pars[["tend"]], 10000)
  expect_equal(pars[["tsave"]], 100)
  expect_equal(pars[["loadarch"]], 0)
  expect_equal(pars[["savepars"]], 1)
  expect_equal(pars[["savelog"]], 0)
  expect_equal(pars[["savearch"]], 1)
  expect_equal(pars[["talkative"]], 1)
  expect_equal(pars[["choose"]], 0)

})

# Test that error when the file does not exist
test_that("Error when file not found", {

  # Check
  expect_error(
    read_pars(path, "nonexistent.txt"),
    paste("file", file.path(path, "nonexistent.txt"), "does not exist")
  )

})

# Test with empty file
test_that("Read empty file", {

  # Read
  pars <- read_pars(path, "empty.txt")

  # Check
  expect_true(is.list(pars))
  expect_true(length(pars) == 0)

})

# Test with empty lines and comments
test_that("Read file with empty lines and comments", {

  # Read
  pars <- read_pars(path, "comments.txt")

  # Check
  expect_true(is.list(pars))
  expect_true(length(pars) == 2)
  expect_true(all(names(pars) == c("foo", "bar")))
  expect_equal(pars[["foo"]], 1)
  expect_equal(pars[["bar"]], 2)

})

# Test with only empty lines and comments
test_that("Read file with only empty lines and comments", {

  # Read
  pars <- read_pars(path, "commentsonly.txt")

  # Check
  expect_true(is.list(pars))
  expect_true(length(pars) == 0)

})

# Test that error when parameter has no value
test_that("Error when parameter has no value", {

  # Check
  expect_error(
    read_pars(path, "novalue.txt"),
    "parameter foo has no value"
  )

})

# Test with conversion of all
test_that("Read with conversion of all", {

  # Read
  pars <- read_pars(path, is_num = "all")

  # Check
  expect_true(is.list(pars))
  expect_equal(length(pars), 26)
  expect_true(all(unlist(lapply(pars, is.numeric))))

})

# Test with no type conversion
test_that("Read with no type conversion", {

  # Read
  pars <- read_pars(path, is_num = "none")

  # Check
  expect_true(is.list(pars))
  expect_equal(length(pars), 26)
  expect_true(all(unlist(lapply(pars, is.character))))
  expect_equal(pars[["type"]], "1")
  expect_equal(pars[["popsize"]], "10")
  expect_true(all(pars[["maxgrowths"]] == c("2", "3")))

})

# Test with custom conversion of some parameters
test_that("Read with conversion to numeric of some parameters", {

  # Read
  pars <- read_pars(path, is_num = c("type", "popsize"))

  # Check
  expect_true(is.list(pars))
  expect_equal(length(pars), 26)
  expect_true(is.numeric(pars[["type"]]))
  expect_true(is.numeric(pars[["popsize"]]))
  expect_true(is.character(pars[["maxgrowths"]]))
  expect_true(is.character(pars[["pgood"]]))
  expect_equal(pars[["type"]], 1)
  expect_equal(pars[["popsize"]], 10)
  expect_true(all(pars[["maxgrowths"]] == c("2", "3")))

})

# Same but to booleans
test_that("Read if conversion to boolean of some parameters", {

  # Read
  pars <- read_pars(path, is_bool = c("savepars", "savelog"))

  # Check
  expect_true(is.list(pars))
  expect_equal(length(pars), 26)
  expect_true(is.logical(pars[["savepars"]]))
  expect_true(is.logical(pars[["savelog"]]))
  expect_true(is.numeric(pars[["maxgrowths"]]))
  expect_true(is.numeric(pars[["pgood"]]))

})

# Test that error when directory has the wrong size
test_that("Error when directory has the wrong size", {

  # Check
  expect_error(
    read_pars(c("foo", "bar")),
    "dir must be of length 1"
  )

})

# Test that error when directory is not character
test_that("Error when directory is not character", {

  # Check
  expect_error(
    read_pars(1),
    "dir must be of class character"
  )

})

# Test that error when filename has the wrong size
test_that("Error when filename has the wrong size", {

  # Check
  expect_error(
    read_pars(path, c("foo", "bar")),
    "filename must be of length 1"
  )

})

# Test that error when filename is not character
test_that("Error when filename is not character", {

  # Check
  expect_error(
    read_pars(path, 1),
    "filename must be of class character"
  )

})

# Test that error when separator has the wrong size
test_that("Error when separator has the wrong size", {

  # Check
  expect_error(
    read_pars(path, sep = c("foo", "bar")),
    "sep must be of length 1"
  )

})

# Test that error when separator is not character
test_that("Error when separator is not character", {

  # Check
  expect_error(
    read_pars(path, sep = 1),
    "sep must be of class character"
  )

})

# Test that error when is_num has the wrong size
test_that("Error when is_num has the wrong size", {

  # Check
  expect_error(
    read_pars(path, is_num = c()),
    "is_num cannot have length 0"
  )

})

# Test that error when is_num is not character
test_that("Error when is_num is not a character", {

  # Check
  expect_error(
    read_pars(path, is_num = 1),
    "is_num must be of class character"
  )

})

# Test that error when is_bool has the wrong size
test_that("Error when is_bool has the wrong size", {

  # Check
  expect_error(
    read_pars(path, is_bool = c()),
    "is_bool cannot have length 0"
  )

})

# Test that error when is_bool is not character
test_that("Error when is_bool is not a character", {

  # Check
  expect_error(
    read_pars(path, is_bool = 1),
    "is_bool must be of class character"
  )

})
