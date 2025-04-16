#' Read simulation data
#'
#' Combine data from one simulation into a single tibble.
#'
#' @param dir Path to the simulation folder
#'
#' @param variables Names of variables to read (e.g. \code{c("time",
#' "popsize")}), with no file extension (i.e. not \code{"time.dat"})
#'
#' @param split Vector of srictly positive integers specifying in how many
#' columns to split each variable (one value per input variable). Note that
#' `0` is not an accepted value. If `split` is a vector of length 1, it will
#' be recycled to the length of \code{variables}.
#'
#' @param dupl Either a vector of strictly positive integers specifying
#' how many times to duplicate each value of each variable (one value per input
#' variable), or a list of vectors of strictly positive integers specifying
#' how many times to duplicate each value of each variable (one vector per
#' input variable). If \code{dupl} is of length 1, it will be recycled to the
#' length of \code{variables}. If \code{dupl} is a vector, then the value `0`
#' is not allowed. If \code{dupl} is a list, then each vector must either be
#' of the same length as its corresponding variable to be read, or of length 1,
#' in which case it will be recycled to the length of the variable.
#'
#' @param type Type of the data to read. Default is \code{"numeric"} but can
#' also be \code{"integer"} or \code{"logical"}.
#'
#' @param size Size of the values to read. Default is \code{8} bytes
#' (size of a double-precision floating point number on most 64-bit systems).
#' Make sure to set this to the correct size for the data type you are reading
#' into (e.g. \code{numeric} are often of size `8`, \code{integer} of size `4`,
#' and \code{logical} can be of size `1`). It is assumed that the size of the
#' values saved into the file is known a priori.
#'
#' @param sep Separator to use for column names when columns are split.
#' Default is \code{"_"}.
#'
#' @param is_tbl Logical. If \code{TRUE}, the result is a tibble.
#' If \code{FALSE}, the result is a data frame. Default is \code{TRUE}.
#'
#' @param ext File extension of the data files. Default is \code{".dat"}.
#' Can be a vector if each variable has its own extension. In that case,
#' \code{ext} must be of the same length as \code{variables}.
#'
#' @param byrow Logical. If \code{TRUE}, the data is fllled into columns
#' by row, otherwise, by column. This should only matter if \code{split} is
#' greater than 1.
#'
#' @details Each variable is read from its respective file into a vector of
#' values. Then, each value of each vector is duplicated a number of times
#' specified by \code{dupl}, and the resulting vector is split into a number of
#' columns specified by \code{split}. The result is one data frame per
#' variable. The data frames are then combined into a single data frame by
#' column, provided that they have the same number of rows (there will be an
#' error otherwise, so \code{split} and \code{dupl} must be chosen wisely).
#'
#' @return A data frame (or tibble) containing the combined data
#'
#' @note The data must be saved in **binary format**, with extension
#' \code{.dat}. See package documentation for details.
#'
#' @examples
#'
#' # Path to a simulation
#' path <- system.file("extdata", "example", package = "readsim")
#'
#' # Read one variable
#' read_data(path, "time")
#'
#' # Read two columns with the same dimensions
#' read_data(path, c("time", "popsize"))
#'
#' # Read two variables but split one into many columns
#' read_data(path, c("time", "patchsizes"), split = c(1, 10))
#'
#' # Read two variables but duplicate one to match the other
#' read_data(path, c("time", "patchsizes"), dupl = c(10, 1))
#'
#' @export
#' @importFrom tibble as_tibble

# Function to read simulation data
read_data <- function(
  dir, variables, split = 1L, dupl = 1L, type = "numeric", size = 8L,
  sep = "_", is_tbl = TRUE, ext = ".dat", byrow = TRUE
) {

  # Check
  checkdir(dir)
  checkvariables(variables)
  checktype(type)
  checksize(size)
  checksep(sep)
  checkistbl(is_tbl)
  checkext(ext, length(variables))

  # Check and format these arguments
  split <- checksplit(split, length(variables))
  dupl <- checkdupl(dupl, length(variables))
  byrow <- checkbyrow(byrow, length(variables))

  # Paths to the data files
  filenames <- file.path(dir, paste0(variables, ext))

  # For each file...
  data <- .mapply(
    function(filename, split, dupl, variable, byrow) {

      # Unlist listed arguments
      dupl <- unlist(dupl)

      # Read the file
      data <- readbinary(filename, type, size)

      # Check
      postcheck(dupl, split, length(data), variable)

      # Format the vector into a table
      data <- format(data, split, dupl, byrow)

      # Rename columns
      colnames(data) <- getcolnames(variable, split, sep)

      # Return
      return(data)

    },
    list(filenames, split, dupl, variables, byrow),
    MoreArgs = NULL
  )

  # Count rows
  nrows <- unlist(lapply(data, nrow))

  # Check
  if (any(nrows != nrows[1]))
    stop("number of rows is not the same across variables")

  # Combine the data into a single table
  data <- do.call(cbind, data)

  # Convert if needed
  if (is_tbl) return(tibble::as_tibble(data))

  # Exit
  return(data)

}

# Function to check for coercibility to integer
is_integer <- function(x) {

  # Check
  if (!is.numeric(x)) return(FALSE)
  if (is.integer(x)) return(TRUE)
  return(all(as.integer(x) == x))

}

# Function to check the split argument
checksplit <- function(split, n) {

  # Check size
  if (!(length(split) %in% c(1, n)))
    stop("split must be of length 1 or the number of variables")

  # Check values
  if (!is_integer(split)) stop("split must be coercible into class integer")
  if (any(split <= 0)) stop("split must only have strictly positive values")

  # Resize if needed
  if (length(split) == 1) split <- rep(split, n)

  # Exit
  return(split)

}

# Function to check the duplication argument
checkdupl <- function(dupl, n) {

  # Check size
  if (!(length(dupl) %in% c(1, n)))
    stop("dupl must be of length 1 or the number of variables")

  # Check format
  islist <- is.list(dupl)

  # If it is a list...
  if (islist) {

    # For each vector...
    lapply(dupl, function(dupl) {

      # Check type
      if (!is_integer(dupl))
        stop("dupl must contain vectors coercible into class integer")

      # Check value
      if (any(dupl < 0)) stop("vector in dupl must only have positive values")

      # Note: zero is allowed here to cover occasional cases where, for example,
      # there may be no individuals in the simulated population at a specific
      # time step, so no individual-level data should be read for that time.

    })

  } else {

    # Check type
    if (!is_integer(dupl))
      stop("if not a list, dupl must be coercible into class integer")

    # Check values
    if (any(dupl <= 0))
      stop("if not a list, dupl must only have strictly positive values")

  }

  # Resize if needed
  if (length(dupl) == 1) dupl <- lapply(seq_len(n), \(x) dupl)
  if (!islist) dupl <- unlist(dupl)

  # Exit
  return(dupl)

}

# Function to check the filling argument
checkbyrow <- function(byrow, n) {

  # Check size
  if (!(length(byrow) %in% c(1, n)))
    stop("byrow must be of length 1 or the number of variables")

  # Check type
  if (!is.logical(byrow)) stop("byrow must be of class logical")

  # Resize if needed
  if (length(byrow) == 1) byrow <- rep(byrow, n)

  # Exit
  return(byrow)

}

# Function to read a binary data file
readbinary <- function(filename, type, size) {

  # Check file
  if (!file.exists(filename)) stop(paste("file", filename, "does not exist"))

  # Early exit
  if (file.size(filename) == 0) return(numeric(0))

  # Check size
  if (file.size(filename) %% size != 0)
    stop(paste("size must divide the size of file", filename))

  # Number of values to read
  n <- file.size(filename) / size

  # Open
  file <- file(filename, "rb")

  # Read
  x <- readBin(file, type, n, size = size)

  # Close
  close(file)

  # Exit
  return(x)

}

# Function to check arguments after having read data
postcheck <- function(dupl, split, n, variable) {

  # Check size
  if (!(length(dupl) %in% c(1, n)))
    stop(paste("wrong length of dupl for variable", variable))

  # Total size after duplication depending on format
  totsize <- ifelse(length(dupl) == 1, dupl * n, sum(dupl))

  # If needed...
  if (totsize %% split != 0) {

    # Error
    stop(paste0(
      "split must divide the number of (duplicated) values for variable ",
      variable, " (length ", n, ")"
    ))

  }
}

# Function to format a vector into a table
format <- function(x, split, dupl, byrow) {

  # Early exit
  if (length(x) == 0) return(data.frame(numeric(0)))

  # Resize if needed
  if (length(dupl) == 1) dupl <- rep(dupl, length(x))

  # Duplicate each value
  x <- rep(x, times = dupl)

  # Split into columns
  x <- matrix(x, ncol = split, nrow = length(x) / split, byrow = byrow)

  # Exit
  return(as.data.frame(x))

}

# Function to rename columns
getcolnames <- function(variable, n, sep) {

  # New names
  colnames <- variable
  if (n > 1) colnames <- paste(variable, seq_len(n), sep = sep)
  return(colnames)

}