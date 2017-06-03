defer
==========================

| CRAN version    | Travis build status   | Coverage |
| :-------------: |:---------------------:|:--------:|
| [![CRAN version](http://www.r-pkg.org/badges/version/defer)](https://cran.r-project.org/package=defer) | [![Build Status](https://travis-ci.org/lbartnik/defer.svg?branch=master)](https://travis-ci.org/lbartnik/defer) | [![codecov](https://codecov.io/gh/lbartnik/defer/branch/master/graph/badge.svg)](https://codecov.io/gh/lbartnik/defer)|


Overview
--------

`defer` wraps functions together with their dependencies so that they
can be sent to another R session and run there with no additional code
overhead. The wrapper itself is a function with the same signature
(that is, R's `formals()`).

-   `defer()` wraps a function and discovers its dependencies
-   `augment()` defines default values for arguments
-   `run_deferred()` is an optional and explicit way to run the wrapper


Installation
------------

Currently `defer` is available only on GitHub.

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("lbartnik/defer")
```


Usage
-----

``` r
library(defer)

wrapper <- defer(function(x) x*x)
wrapper
#> Deferred-execution package
#> 
#> Entry function:
#>   function (x) 
#>   x * x

wrapper(10)
#> [1] 100
```


When wrapping a more elaborate pipeline:

``` r
C <- 10

f <- function(x) x*x + C
g <- function(y) (f(y) + 10) * f(y+3)
h <- function(z) mean(c(g(z), g(z+1), g(z+2)))

wrapper <- defer(h)
#> Found functions:
#>   g, f
#> variables:
#>   C
#> library calls:
#>   base::mean

rm(C, f, g, h)

wrapper(10)
#> [1] 29688.67
```


How about executing this last `wrapper()` via [`opencpu`](http://www.opencpu.org)?
It's enough to:

-   serialize the wrapper and turn the byte array into Base64-encoded ASCII string
-   put the string in a R snippet that restores the original R function object
-   run the function

(See the _Examples_ vignette for more details.)


``` r
library(httr)
library(base64enc)

public_opencpu_url <- "https://cloud.opencpu.org/ocpu/library/base/R/source/print"
local_script_path <- tempfile(fileext = ".R")

buffer <- rawConnection(raw(0), 'w')
saveRDS(wrapper, buffer)
wrapper_base64 <- base64enc::base64encode(rawConnectionValue(buffer))

cat(paste0("base64 <- '", wrapper_base64, "'\n",
           "library(base64enc)\n",
           "bytes <- rawConnection(base64enc::base64decode(base64), 'r')\n",
           "deserialized <- readRDS(bytes)\n",
           "deserialized(10)\n"),
    file = local_script_path)

http_result <- httr::POST(public_opencpu_url,
                          body = list(file = upload_file(local_script_path)))
content(http_result, 'text')
#> [1] "$value\n[1] 29688.67\n\n$visible\n[1] TRUE\n\n"
```
