
<!-- README.md is generated from README.Rmd. Please edit that file -->

# htmlreportR

<!-- badges: start -->

[![R-CMD-check](https://github.com/AEstebanMar/htmlreportR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AEstebanMar/htmlreportR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

htmlreportR has not been built from scratch. It is based on its Python
equivalent and sister project, available at
<https://pypi.org/project/py-report-html/> for a Python equivalent.

## Installation

Standard installation:

``` r
install.packages("htmlreportR")
```

You can install the development version of htmlreportR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AEstebanMar/htmlreportR")
```

## Example

There are two use cases for htmlreportR: script mode and package mode.

### Script mode

Simply call the html\_report.R script, distributed with this package.
Simply provide a comma-separated list of files and a template to render,
and you’re done\!

``` bash
./scripts/html_report.R -d data1.txt,data2.txt -t template.txt
```

### Library mode

If you wish to use htmlreportR directly from R, you can\! Here’s how:

``` r
library(htmlreportR)
container <- list(data1 = data_frame_1, data2 = data_frame_2,
                  additional_field = "You can include anything in the container
                  and it will be available in plotter$hash_vars")
plotter <- htmlReport$new(title_doc = "Your report title", 
                          container = container, tmp_folder = "tmp_lib",
                          src = "path/to/htmlreportR/scripts")
```
