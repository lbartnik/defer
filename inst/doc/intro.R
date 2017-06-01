## ----setup, include=FALSE------------------------------------------------
library(knitr)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----minimalistic--------------------------------------------------------
library(defer)

fun <- function(x)x*x
deferred <- defer(fun)
print(deferred)

deferred(10)

## ----echo=FALSE----------------------------------------------------------
rm(fun, deferred)

## ----verify, echo=FALSE--------------------------------------------------
verify <- function (model, test_data) {
  test_data$predicted <- predict(model, test_data) > .5
  with(test_data, predicted == is_setosa)
}

## ----model, echo=FALSE---------------------------------------------------
model <- function (train_data) {
  lm(is_setosa ~ petal_area + sepal_area, data = train_data)
}

## ----etl, echo=FALSE-----------------------------------------------------
etl <- function (data) {
  names(data) <- tolower(names(data))
  names(data) <- gsub("\\.", "_", names(data))
  
  data$sepal_area    <- with(data, sepal_width * sepal_length)
  data$petal_area    <- with(data, petal_width * petal_length)
  data$is_setosa     <- data$species == "setosa"
  data$is_virginica  <- data$species == "virginica"
  data$is_versicolor <- data$species == "versicolor"
  data$species       <- NULL
  
  data
}

## ------------------------------------------------------------------------
glue <- function (data, test_size) {
  data  <- etl(data)
  test  <- sample.int(nrow(data), test_size)
  train <- setdiff(seq(nrow(data)), test)
  
  m <- model(data[train, ])
  mean(verify(m, data[test, ]))
}

## ------------------------------------------------------------------------
glue(iris, 50)

## ------------------------------------------------------------------------
library(defer)

d <- defer(glue)

## ------------------------------------------------------------------------
# serialize
storage_path <- tempfile(fileext = 'rds')
saveRDS(d, storage_path)

# removing these functions "simulates" a new R session
rm(d, glue, etl, verify, model)
ls()

# deserialize and run
d <- readRDS(storage_path)
d(iris, 50)

## ----verify--------------------------------------------------------------
verify <- function (model, test_data) {
  test_data$predicted <- predict(model, test_data) > .5
  with(test_data, predicted == is_setosa)
}

## ----model---------------------------------------------------------------
model <- function (train_data) {
  lm(is_setosa ~ petal_area + sepal_area, data = train_data)
}

## ----etl-----------------------------------------------------------------
etl <- function (data) {
  names(data) <- tolower(names(data))
  names(data) <- gsub("\\.", "_", names(data))
  
  data$sepal_area    <- with(data, sepal_width * sepal_length)
  data$petal_area    <- with(data, petal_width * petal_length)
  data$is_setosa     <- data$species == "setosa"
  data$is_virginica  <- data$species == "virginica"
  data$is_versicolor <- data$species == "versicolor"
  data$species       <- NULL
  
  data
}

