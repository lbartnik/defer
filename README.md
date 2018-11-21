defer
==========================

| CRAN version    | Travis build status   | AppVeyor | Coverage |
| :-------------: |:---------------------:|:--------:|:--------:|
| [![CRAN version](http://www.r-pkg.org/badges/version/defer)](https://cran.r-project.org/package=defer) | [![Build Status](https://travis-ci.org/lbartnik/defer.svg?branch=master)](https://travis-ci.org/lbartnik/defer) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lbartnik/defer?branch=master&svg=true)](https://ci.appveyor.com/project/lbartnik/defer) | [![codecov](https://codecov.io/gh/lbartnik/defer/branch/master/graph/badge.svg)](https://codecov.io/gh/lbartnik/defer)|


Overview
--------

`defer` wraps a function together with its dependencies. The result is
a self-contained function that can be:

-   stored for documentation purposes
-   sent over to a different (possibly remote) R session and run there
    without elaborate dependency restoring

The wrapper itself is a function with the same signature (that is,
`formals()`).

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

(See the [Examples](inst/doc/examples.html) vignette for more details.)

The best thing is that `opencpu` does not have to provide the `defer` package
because the wrapper is self-contained!


``` r
library(httr)

public_opencpu_url <- "https://cloud.opencpu.org/ocpu/library/base/R/source/print"

# we're still using the same wrapper object as above
serialized_wrapper <- jsonlite::base64_enc(serialize(wrapper, NULL))

local_script_path <- tempfile(fileext = ".R")
cat(paste0("wrapper <- unserialize(jsonlite::base64_dec('", serialized_wrapper, "'))\n",
           "wrapper(10)\n"),
    file = local_script_path)

http_result <- httr::POST(public_opencpu_url,
                          body = list(file = upload_file(local_script_path)))
content(http_result, 'text')
#> [1] "$value\n[1] 29688.67\n\n$visible\n[1] TRUE\n\n"
```
