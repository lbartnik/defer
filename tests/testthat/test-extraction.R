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

