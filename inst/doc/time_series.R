## ----setup, include=FALSE------------------------------------------------
library(knitr)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(magrittr)

# data: timestamp and value
etl <- function (data) {
  # replace NAs with the average of surrounding values
  data %<>%
    mutate(lead_lag_mean = (lead(value, 1) + lag(value, 1))/2)
  i <- is.na(data$value)
  data$value[i] <- data$lead_lag_mean[i]

  # cast timestapm, add lags
  data %>%
    select(-lead_lag_mean) %>%
    mutate(timestamp = floor_date(timestamp, "hour")) %>%
    group_by(timestamp) %>%
    summarise(value = mean(value)) %>%
    ungroup %>%
    mutate(lag_1 = lag(value, 1),
           lag_2 = lag(value, 2),
           lag_3 = lag(value, 3))
}

score <- function (data, n) {
  train <- tail(data, n = -n)
  test  <- tail(data, n = n)[1, ]
  
  m <- lm(value ~ ., data = train)
  test$value - predict(m, test)
}

glue <- function (data, n = 10) {
  data <- etl(data)
  ans <- lapply(seq(n), function (n) {
    score(data, n)
  })
  test_residuals <- unlist(ans)
  sqrt(mean(test_residuals ^ 2))
}

## ------------------------------------------------------------------------
n_samples <- 4*96
time_series <- data_frame(timestamp = as.POSIXct("2017-05-01") + seq(n_samples) * 900,
                          value = rnorm(n_samples))

## ------------------------------------------------------------------------
library(defer)
deferred_glue <- defer_(glue, score = score, etl = etl)

## ------------------------------------------------------------------------
deferred_glue(time_series)

## ------------------------------------------------------------------------
saveRDS(deferred_glue, "glue.rds")
rm(glue, etl, score, deferred_glue)

## ------------------------------------------------------------------------
deferred_glue <- readRDS("glue.rds")
deferred_glue(time_series)

