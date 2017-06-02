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
augment(deferred_fun, x = 7, y = 3)

## ------------------------------------------------------------------------
deferred_fun()

## ------------------------------------------------------------------------
library(httr)
library(base64enc)

public_opencpu_url <- "https://cloud.opencpu.org/ocpu/library/base/R/source/print"
local_script_path <- tempfile(fileext = ".R")

## ----opencpu_call,eval=FALSE---------------------------------------------
#  http_result <- httr::POST(public_opencpu_url,
#                            body = list(file = upload_file(local_script_path)))

## ----to_base64, echo=FALSE-----------------------------------------------
to_base64 <- function (object)
{
  rds_buffer <- rawConnection(raw(0), 'w')
  on.exit(close(rds_buffer))
  saveRDS(object, rds_buffer)
  base64enc::base64encode(rawConnectionValue(rds_buffer))
}

## ----opencpu_script------------------------------------------------------
cat(paste0("base64 <- '", to_base64(deferred_fun), "'\n",
           "library(base64enc)\n",
           "bytes <- rawConnection(base64enc::base64decode(base64), 'r')\n",
           "deserialized <- readRDS(bytes)\n",
           "deserialized()\n"),
    file = local_script_path)

## ----to_base64-----------------------------------------------------------
to_base64 <- function (object)
{
  rds_buffer <- rawConnection(raw(0), 'w')
  on.exit(close(rds_buffer))
  saveRDS(object, rds_buffer)
  base64enc::base64encode(rawConnectionValue(rds_buffer))
}

## ------------------------------------------------------------------------
http_result <- httr::POST(public_opencpu_url,
                          body = list(file = upload_file(local_script_path)))
content(http_result, 'text')

