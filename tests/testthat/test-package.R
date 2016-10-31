context("package")


# --- packaging --------------------------------------------------------

test_that("execution package is returned and function can be listed", {
  f   <- function(){}
  pkg <- package_(f)
  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), 'entry')
})


test_that("multiple functions can be packaged", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)
  pkg <- package_(f, g = g)

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), c('entry', 'g'))
})


test_that("entry can be name of a function", {
  f <- function(){}

  pkg <- package_(entry = 'f', f = f)
  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), 'f')

  expect_error(package_(entry = 'f', g = f))
})


test_that("only named functions are allowed", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)

  expect_error(package_(f, g))
  expect_error(package_(f, 1))
})


test_that("library functions are not packaged but recorded", {
  pkg <- package_(function(){}, summary = summary)

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), 'entry')
  expect_equal(list_dependencies(pkg, c(base = 'summary')))
})


# --- execution --------------------------------------------------------

test_that("package can be executed", {
  f   <- mock(1, 2)
  pkg <- package_(f)

  expect_equal(run_package(pkg, 1, 2), 1)
  expect_equal(run_package(pkg, 3, 4), 2)

  expect_no_calls(f, 2)
  expect_args(f, 1, 1, 2)
  expect_args(f, 1, 3, 4)
})


test_that("other function can be executed", {
  f   <- mock(1)
  pkg <- package_(function(...){}, f = f)

  expect_equal(run_package(pkg, .fun = 'f', 1, 2), 1)
  expect_no_calls(f, 1)
  expect_args(f, 1, 1, 2)
})


test_that("named entry function can be executed", {
  f   <- mock(1)
  pkg <- package_('f', f = f)

  expect_equal(run_package(pkg, 1, 2), 1)
  expect_no_calls(f, 1)
  expect_args(f, 1, 1, 2)
})


# --- complex execution -------------------------------------------------

test_that("functions can execute each other", {
  m <- mock(1)
  f <- function(...) g(...)

  # mock is passed under a different name to verify that f() doesn't reach
  # back to *this* environment
  pkg <- package_('f', g = m)

  expect_equal(run_package(pkg, 1, 2), 1)
  expect_no_calls(f, 1)
  expect_args(f, 1, 1, 2)
})

