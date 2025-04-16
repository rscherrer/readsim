#' Read simulation parameters
#'
#' Read the parameter file in a simulation folder
#'
#' @param dir Path to the simulation folder
#' @param filename Name of the parameter file
#' @param sep Separator between parameter names and values
#' @param is_num,is_bool Either optional names of parameters to convert to
#' numeric or logical, respectively, or \code{"all"} for all (default of
#' \code{is_num}), or \code{"none"} (default of \code{is_bool}) for no
#' conversion. Note that conversion to logical will override conversion to
#' numeric.
#'
#' @details The function looks for a parameter text file in the folder, and
#' reads each line. The first entry of each line must be the parameter name,
#' and the rest of the line a list of values. The values are separated by
#' a given separator (default is a space). The function returns a list of
#' values (or vectors of values if applicable) named after their respective
#' parameter.
#'
#' @return A named list of parameter values
#'
#' @examples
#'
#' # Path to a simulation folder
#' path <- system.file("extdata", "example", package = "readsim")
#'
#' # Read parameters
#' read_pars(path)
#'
#' # Read parameters with custom conversion
#' read_pars(path, is_num = c("popsize", "maxgrowths"), is_bool = "savepars")
#'
#' @export

# Function to read parameters from a text file
read_pars <- function(
  dir, filename = "parameters.txt", sep = " ", is_num = "all",
  is_bool = "none"
) {

  # Check
  checkdir(dir)
  checkfilename(filename)
  checksep(sep)
  checkisnum(is_num)
  checkisbool(is_bool)

  # Set the parameter file name
  filename <- file.path(dir, filename)

  # Error if the file does not exist
  if (!file.exists(filename))
    stop(paste("file", filename, "does not exist"))

  # Early exit if the file is empty
  if (file.info(filename)$size == 0) return(list())

  # Read the lines in the parameter file
  lines <- tryread(filename)

  # Remove comments (lines starting with #)
  lines <- gsub("^#.*", "", lines)

  # Remove leading and trailing separators
  lines <- gsub(paste0("^", sep, "+|", sep, "+$"), "", lines)

  # For each parameter...
  lines <- lapply(lines, function(line) {

    # Split name from values
    line <- strsplit(line, sep)[[1]]

    # Remove occasional empty string
    line[line != ""]

  })

  # Remove empty lines
  lines <- lines[unlist(lapply(lines, length)) != 0]

  # Exit if nothing left
  if (length(lines) == 0) return(list())

  # Check that values are given
  checkpars(lines)

  # Extract parameter names
  parnames <- as.character(unlist(lapply(lines, \(x) x[1])))

  # Extract parameter values
  pars <- lapply(lines, \(x) x[-1])

  # Add names
  names(pars) <- parnames

  # Convert some parameters if needed
  pars <- convert(pars, is_num, as.numeric)
  pars <- convert(pars, is_bool, as.logical)

  # Exit
  return(pars)

}

# Function to read lines from the file
tryread <- function(filename) {

  # Open file
  file <- file(filename, "r")

  # Try to...
  lines <- withCallingHandlers({

    # Read the lines
    readLines(file)

  }, warning = function(w) {

    # Suppress warning if needed
    if (grepl("incomplete final line", conditionMessage(w)))
      invokeRestart("muffleWarning")

  })

  # Close the file
  close(file)

  # Exit
  return(lines)

}

# Function to check the validity of entries
checkpars <- function(lines) {

  # Count entries on each line
  nentries <- unlist(lapply(lines, length))

  # Problematic entries
  wrong <- nentries == 1

  # Error if needed
  if (any(wrong))
    stop(paste("parameter", lines[which(wrong)[1]], "has no value"))

}

# Function to apply a transformation to certain parameters
convert <- function(pars, which, fun) {

  # No conversion by default
  ii <- rep(FALSE, length(pars))

  # Set some if needed
  ii[names(pars) %in% which] <- TRUE

  # Set all if requested
  if (which[1] == "all") ii <- rep(TRUE, length(pars))

  # Convert to numeric
  pars[ii] <- lapply(pars[ii], fun)

  # Exit
  return(pars)

}
