#' Write mock data
#'
#' Write a vector of values into a binary file.
#'
#' @param values Vector of numeric values to write
#' @param filename Name of the file to write to
#'
#' @return Nothing (saves to file)
#'
#' @note This function is for testing purposes mostly, and can be used to
#' play with the `read_data` function.
#'
#' @examples
#'
#' # Write mock data
#' values <- rnorm(100)
#' write_data(1:10, "mock.dat")
#'
#' # Read the data back
#' read_data(dir = ".", "mock")
#'
#' # Delete the file
#' file.remove("mock.dat")
#'
#' @export

# Function to write a mock data file
write_data <- function(values, filename) {

  # values: numeric vector of values
  # filename: name of the file to write to

  # Check
  if (length(values) == 0) stop("values cannot have length 0")
  if (!is.numeric(values)) stop("values must be of class numeric")
  if (length(filename) != 1) stop("filename must have length 1")
  if (!is.character(filename)) stop("filename must be of class character")

  # Open
  con <- file(filename, "wb")

  # Write
  writeBin(values, con)

  # Close
  close(con)

}