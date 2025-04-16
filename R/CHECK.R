## This file contains argument checkers

# Function to check the directory
checkdir <- function(dir) {

  # Check
  if (length(dir) != 1) stop("dir must be of length 1")
  if (!is.character(dir)) stop("dir must be of class character")

}

# Function to check the variables
checkvariables <- function(variables) {

  # Check
  if (length(variables) == 0) stop("variables cannot have length 0")
  if (!is.character(variables)) stop("variables must be of class character")

}

# Function to check the type
checktype <- function(type) {

  # Check
  if (length(type) != 1) stop("type must be of length 1")
  if (!is.character(type)) stop("type must be of class character")
  if (!(type %in% c("numeric", "integer", "logical")))
    stop("type must be one of numeric, integer, or logical")

}

# Function to check the size
checksize <- function(size) {

  # Check
  if (length(size) != 1) stop("size must be of length 1")
  if (!is_integer(size)) stop("size must be coercible into class integer")
  if (size <= 0) stop("size must be strictly positive")

}

# Function to check the extension
checkext <- function(ext, n) {

  # Check
  if (!(length(ext) %in% c(1, n)))
    stop("ext must be of length 1 or the number of variables")
  if (!is.character(ext)) stop("ext must be of class character")

}

# Function to check the name separator
checksep <- function(sep) {

  # Check
  if (length(sep) != 1) stop("sep must be of length 1")
  if (!is.character(sep)) stop("sep must be of class character")

}

# Function to check the tibble conversion argument
checkistbl <- function(is_tbl) {

  if (length(is_tbl) != 1) stop("is_tbl must be of length 1")
  if (!is.logical(is_tbl)) stop("is_tbl must be of class logical")

}

# Function to check the file name
checkfilename <- function(filename) {

  # Check
  if (length(filename) != 1) stop("filename must be of length 1")
  if (!is.character(filename)) stop("filename must be of class character")

}

# Function to check the numeric conversion parameters
checkisnum <- function(is_num) {

  # Check
  if (length(is_num) == 0) stop("is_num cannot have length 0")
  if (!is.character(is_num)) stop("is_num must be of class character")

}

# Function to check the boolean conversion parameters
checkisbool <- function(is_bool) {

  # Check
  if (length(is_bool) == 0) stop("is_bool cannot have length 0")
  if (!is.character(is_bool)) stop("is_bool must be of class character")

}