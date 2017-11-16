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

})
