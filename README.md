# readsim

This is an [R](https://www.r-project.org/) package used to read data from separate files and combine them in tables in a modular way. It is particularly suited for simulated data.

![Tests](https://img.shields.io/badge/tests-passing-brightgreen)
![Coverage](https://img.shields.io/badge/coverage-100%25-brightgreen)
![Check](https://img.shields.io/badge/check-succeeded-brightgreen)

## Description

This package provides one key function, `read_data()`, which can be used to combine data from various files in a flexible way. It is made for programs, such as **individual-based simulations, that save various outputs, to be combined in various and modular ways** depending on how they must be analyzed. This makes easy the combination of multiple variables from multiple files, often with different dimensions or units of observation.

The package is made to read **binary files**, as they are a fast and light way to write numerical variables separately in a way that they can be later combined as desired.

## Installation

The package can be installed from GitHub:

```r
devtools::install_github("rscherrer/readsim", build_vignettes = TRUE)
```

This will allow to browse the vignettes of the package using `vignette()`.

## Example

Simulation data can be complex, and a given study may require an analyses of a thousand simulations recording some population-level data at every generation, for example (in `.csv` format):

```csv
time, popsize,
1, 10,
2, 23,
...
```

The same study may require the data to be assembled in a different way, involving a very different unit of observation, for example, one row per individual of every generation:

```csv
time, popsize, individual, trait, habitat,
1, 10, 1, 0.65, 3,
1, 10, 2, 0.87, 2,
1, 10, 3, 0.52, 3,
...
```

Both data set have some columns in common (`time` and `popsize`). Multiplied by possibly thousands of observations, thousands of replicate simulations, and more than two types of data sets per simulation, saving the data in separate `.csv` files of this kind has the potential of generating a lot of redundant information.

One alternative way of saving the data is into separate files (in binary format, for speed), where each variable is uniquely represented:

```
folder/
|-- time.dat
|-- popsize.dat
|-- individual.dat
|-- trait.dat
|-- habitat.dat
```

Of course, now, each file may have its own unit of observation and its own size. **Combining them into meaningful tables from user-specified rules, readily analyzable in R** (such as the data sets shown above), is the goal of this package.

For more information and concrete use cases, please refer to the main vignette in R:

```r
vignette("use-read-data", package = "readsim")
```

## See also

There is also a `read_pars()` function that allows to read parameter text files. See `?read_pars()` for details, or refer to the following vignette:

```r
vignette("use-read-pars", package = "readsim")
```

## Dependencies

Check `Imports` in the `DESCRIPTION` file for a list of packages (and minimum versions required) listed as dependencies (other than core R packages) for this package.

## About

This package was developed on Ubuntu Linux 24.04 LTS, in [RStudio](https://posit.co/download/rstudio-desktop/) 2024.12.1 and [Visual Studio Code](https://code.visualstudio.com/) 1.99.0 ([R extension](https://marketplace.visualstudio.com/items/?itemName=REditorSupport.r) 2.8.5), with [R](https://www.r-project.org/) 4.3.3 (compiled with [GCC](https://gcc.gnu.org/) 13.2.0), and making use of packages [covr](https://covr.r-lib.org/) 3.6.4, [devtools](https://devtools.r-lib.org/) 2.4.5, [knitr](https://cran.r-project.org/web/packages/knitr/index.html) 1.49, [languageserver](https://cran.r-project.org/web/packages/languageserver/index.html) 0.3.16, [lintr](https://lintr.r-lib.org/) 3.1.1, [rmarkdown](https://cran.r-project.org/web/packages/rmarkdown/index.html) 2.29, [roxygen2](https://roxygen2.r-lib.org/) 7.3.1, [testthat](https://testthat.r-lib.org/) 3.0.0 and [usethis](https://usethis.r-lib.org/) 2.2.2. Occasional use was made of [ChatGPT](https://chatgpt.com/) and [GitHub Copilot](https://github.com/features/copilot) in the development of this code.

## Permissions

Copyright (c) 2025 Raphaël Scherrer.

This code is licensed under the [MIT License](LICENSE.md). You are free to use, modify, and distribute it, provided that the original copyright notice and permission notice are included in all copies or substantial portions of the software.
