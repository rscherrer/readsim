#!/usr/bin/env Rscript

## This script checks the code coverage of the package using the covr package.
## It requires the covr package to be installed.

# Load covr
if (!requireNamespace("covr", quietly = TRUE))
  stop("the 'covr' package is required but not installed")

# Check package coverage
coverage <- covr::package_coverage()

# Show
print(coverage)

# Generate an HTML report
covr::report(coverage)