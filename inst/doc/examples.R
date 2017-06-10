## ----setup, include=FALSE------------------------------------------------
library(knitr)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----original------------------------------------------------------------
fun <- function(x, y) x ^ y

## ------------------------------------------------------------------------
library(defer)

deferred_fun <- defer(fun)
deferred_fun(9, 2)

## ------------------------------------------------------------------------
deferred_fun <- augment(deferred_fun, x = 7, y = 3)

## ------------------------------------------------------------------------
deferred_fun()

## ------------------------------------------------------------------------
library(httr)
library(jsonlite)

public_opencpu_url <- "https://cloud.opencpu.org/ocpu/library/base/R/source/print"
local_script_path <- tempfile(fileext = ".R")

## ----opencpu_call,eval=FALSE---------------------------------------------
#  http_result <- httr::POST(public_opencpu_url,
#                            body = list(file = upload_file(local_script_path)))

## ----opencpu_script------------------------------------------------------
serialized_wrapper <- jsonlite::base64_enc(serialize(deferred_fun, NULL))
cat(paste0("bytes <- unserialize(jsonlite::base64_dec('", serialized_wrapper, "'))\n",
           "deserialized()\n"),
    file = local_script_path)

## ------------------------------------------------------------------------
http_result <- httr::POST(public_opencpu_url,
                          body = list(file = upload_file(local_script_path)))
content(http_result, 'text')

