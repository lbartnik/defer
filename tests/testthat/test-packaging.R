context("packaging")


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
  d <- defer_(function(...)f(), f = function()x, x = 1)
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


test_that("load library deps", {
  unloadNamespace("MASS")
  expect_false(isNamespaceLoaded("MASS"))

  d <- defer_(function(x, y)area(sin, x, y), MASS::area)
  expect_is(d, "deferred")
  expect_equal(d(0, 1), 0.4596977)
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


test_that("formals with ...", {
  d <- defer_(function(...)list(...))
  expect_is(d, "deferred")
  expect_equal(d(1), list(1))
  expect_equal(d(a = 1), list(a = 1))
})


test_that("formals with named args and ...", {
  d <- defer_(function(a, ...)list(a = a, ...))
  expect_is(d, "deferred")
  expect_equal(d(1), list(a = 1))
  expect_equal(d(a = 1), list(a = 1))
  expect_equal(d(a = 1, 2), list(a = 1, 2))
  expect_equal(d(a = 1, b = 2), list(a = 1, b = 2))
})


test_that("%>% is recognized as regular dependency", {
  skip_if_not_installed("magrittr")
  
  d <- defer_(function(x)f(x), f = . %>% abs)
  
  expect_is(d, 'deferred')
  expect_setequal(extract_functions(d), c('entry', 'f'))
  expect_equal(d(-1), 1)
})


test_that("handling errors in .dots", {
  # unnamed
  expect_error(defer_(function(){}, .dots = list(function(){})),
               "some elements in `.dots` are not named and names cannot be auto-generated")
  
  # not a function
  expect_error(defer_(function(){}, 1),
               "some arguments passed in ... are not named and names cannot be auto-generated")
  
  # name conflict
  expect_error(defer_(function(){}, f = function(){}, .dots = list(f = function(){})),
               "names in ... and `.dots` cannot overlap")
})


test_that("simple execution", {
  f   <- mock(1, 2)
  d <- defer_(f)

  expect_is(d, "deferred")
  
  expect_equal(d(1, 2), 1)
  expect_equal(d(3, 4), 2)
  
  expect_called(f, 2)
  expect_args(f, 1, 1, 2)
  expect_args(f, 2, 3, 4)
})


test_that("nested execution", {
  m <- mock(1)
  f <- function(...) g(...)
  
  # mock is passed under a different name to verify that f() doesn't reach
  # back to *this* environment
  d <- defer_(f, g = m)
  
  expect_is(d, "deferred")
  expect_equal(d(1, 2), 1)
  expect_called(m, 1)
  expect_args(m, 1, 1, 2)
})


test_that("drop global env", {
  f <- function(){}
  environment(f) <- globalenv()
  
  d <- defer_(f)
  expect_is(d, "deferred")
  expect_identical(environment(environment(d)$function_deps$entry), emptyenv())
})

