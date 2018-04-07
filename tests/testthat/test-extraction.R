context("automatic extraction")

test_that("function", {
  f <- function()1
  d <- defer_(function()f(), .extract = TRUE)

  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("variable", {
  x <- 1
  d <- defer_(function()x, .extract = TRUE)

  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("function and variable", {
  x <- 1
  f <- function()x
  d <- defer_(function()f(), .extract = TRUE)

  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("from environment", {
  f <- function() x <- y
  e <- as.environment(list(y = 1))
  d <- defer_(f, .caller_env = e, .extract = TRUE)
  v <- extract_variables(d)
  expect_length(v, 1)
  expect_named(v, 'y')
})


test_that("different types", {
  extracts <- function (value) {
    f <- function() x <- y
    e <- as.environment(list(y = value))
    d <- defer_(f, .caller_env = e, .extract = TRUE)
    v <- extract_variables(d)
    expect_length(v, 1)
    expect_named(v, 'y')
    expect_equal(v[[1]], value)
  }

  extracts(1)
  extracts("a")
})


test_that("with dollar sign", {
  f <- function() iris2$Sepal.Length <- iris2$Sepal.Length^2
  e <- as.environment(list(iris2 = 1))
  d <- defer_(f, .caller_env = e, .extract = TRUE)
  v <- extract_variables(d)
  expect_length(v, 1)
  expect_named(v, 'iris2')
})


test_that("only rhs", {
  f <- function() x <- y
  e <- as.environment(list(x = 1, y = '2'))
  d <- defer_(f, .caller_env = e, .extract = TRUE)
  v <- extract_variables(d)
  expect_named(v, "y")
})


test_that("fseq can be packaged", {
  skip_if_not_installed("magrittr")

  f <- . %>% summary(.)
  d <- defer_(f, .extract = TRUE)

  v <- extract_dependencies(d)
  expect_equal(nrow(v), 2)
  expect_setequal(v$fun, c("%>%", "summary"))
})


test_that("pipe operators are recognized and processed", {
  skip_if_not_installed("magrittr")
  require("magrittr", quietly = TRUE)

  # %>%
  f <- function (x) { x %>% summary }
  d <- defer_(f, .extract = TRUE, .verbosity = 2)

  v <- extract_dependencies(d)
  expect_equal(nrow(v), 2)
  expect_setequal(v$fun, c("%>%", "summary"))

  # %<>%
  f <- function (x) { x %<>% summary }
  d <- defer_(f, .extract = TRUE, .verbosity = 2)

  v <- extract_dependencies(d)
  expect_equal(nrow(v), 2)
  expect_setequal(v$fun, c("%<>%", "summary"))
})


test_that("complex pipe", {
  skip_if_not_installed("magrittr")
  require("magrittr", quietly = TRUE)

  f <- function () {
    input <- system.file("extdata/block_62.csv", package = "experiment") %>%
      readr::read_csv(na = "Null") %>%
      dplyr::rename(meter = LCLid, timestamp = tstp, usage = energy_kWh) %>%
      dplyr::filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
                    lubridate::year(timestamp) == 2013)
  }
  e <- new.env()
  d <- defer_(f, .caller_env = e, .extract = TRUE, .verbosity = 2)

  v <- extract_dependencies(d)
  expect_equal(nrow(v), 7)
  expect_setequal(v$fun, c("%>%", "%in%", "system.file", "read_csv", "rename", "filter", "year"))
  expect_setequal(v$pkg, c("magrittr", "base", "devtools", "readr", "dplyr", "lubridate"))
})


test_that("compound pipe", {
  skip_if_not_installed("magrittr")
  requireNamespace("magrittr", quietly = TRUE)

  f <- function () input %<>% base::summary(meter == "MAC004929")
  e <- new.env()
  d <- defer_(f, .caller_env = e, .extract = TRUE, .verbosity = 2)

  v <- extract_dependencies(d)
  str(v)
  expect_equal(nrow(v), 2)
  expect_setequal(v$pkg, c("base", "magrittr"))
})
