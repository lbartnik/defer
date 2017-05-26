context("packaging")


test_that("single function", {
  d <- defer(function()1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("single function dependency", {
  d <- defer(function()f(), f = function()1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("single variable dependency", {
  d <- defer(function()x, x = 1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("variable and function", {
  d <- defer(function()f(), f = function()x, x = 1)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("by name", {
  f <- function()x
  x <- 1
  d <- defer(function()f(), f = f, x = x)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("just name", {
  f <- function()x
  x <- 1
  d <- defer(function()f(), f, x)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("dots", {
  d <- defer(function()f(), .dots = list(f = function()x, x = 1))
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("dots by name", {
  f <- function()x
  x <- 1
  q <- quos(x = x, f = f)

  d <- defer(function()f(), .dots = q)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})


test_that("unnamed dots", {
  f <- function()x
  x <- 1
  q <- quos(x, f)
  
  d <- defer(function()f(), .dots = q)
  expect_is(d, "deferred")
  expect_equal(d(), 1)
})

