context("non-interactive")


test_that("single function", {
  d <- defer_(function()1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("single function dependency", {
  d <- defer_(function()f(), f = function()1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("single variable dependency", {
  d <- defer_(function()x, x = 1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("variable and function", {
  d <- defer_(function()f(), f = function()x, x = 1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("by name", {
  f <- function()x
  x <- 1
  d <- defer_(function()f(), f = f, x = x)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("just name", {
  f <- function()x
  x <- 1
  d <- defer_(function()f(), f, x)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("double colon", {
  d <- defer_(function()summary(1), base::summary)
  expect_is(d, "deferred")
  expect_equal(d(), summary(1))
})


test_that("dots", {
  d <- defer_(function()f(), .dots = list(f = function()x, x = 1))
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("dots by name", {
  f <- function()x
  x <- 1
  q <- quos(x = x, f = f)

  d <- defer_(function()f(), .dots = q)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("unnamed dots", {
  f <- function()x
  x <- 1
  q <- quos(x, f)
  
  d <- defer_(function()f(), .dots = q)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("extract function", {
  f <- function()1
  d <- defer_(function()f(), .extract = TRUE)

  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("extract variable", {
  x <- 1
  d <- defer_(function()x, .extract = TRUE)
  
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("extract function and variable", {
  x <- 1
  f <- function()x
  d <- defer_(function()f(), .extract = TRUE)
  
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})
