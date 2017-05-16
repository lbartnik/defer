context("packaging")


# --- packaging --------------------------------------------------------

test_that("execution package is returned and function can be listed", {
  f   <- function(){}
  pkg <- defer_(f)
  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), 'entry')
})


test_that("multiple functions can be packaged", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)
  pkg <- defer_(f, g = g)

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), c('entry', 'g'))
})


test_that("only named functions are allowed", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)

  expect_error(defer_(f, g))
  expect_error(defer_(f, 1))
})


test_that("library functions are not packaged but recorded", {
  pkg <- defer_(function(){}, summary = summary)

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), 'entry')
  expect_equal(list_dependencies(pkg), c(base = 'summary'))
})


test_that("%>% is recognized as regular dependency", {
  skip_if_not_installed("magrittr")

  pkg <- defer_(function(){}, f = . %>% summary(.))

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), c('entry', 'f'))
})

# --- passing functions through .funcs ---------------------------------

test_that("functions can be passed by .funcs", {
  pkg <- defer_(function(x)f(x), .funcs = list(f = function(y)summary(y)))

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), c('entry', 'f'))
  expect_equal(run_package(pkg, iris), summary(iris))
})


test_that("handling errors in .funcs", {
  # unnamed
  expect_error(defer_(function(){}, .funcs = list(function(){})),
               "all elements in .funcs need to be named")

  # not a function
  expect_error(defer_(function(){}, .funcs = list(f = 1)),
               "only functions can be passed via .funcs")

  # name conflict
  expect_error(defer_(function(){}, f = function(){}, .funcs = list(f = function(){})),
               "names in ... and .funcs cannot overlap")
})


# --- unnamed library hints --------------------------------------------

test_that("unnamed symbols can be passed in ...", {
  skip("this functionality is not yet implemented")

  # TODO to implement make a change in make_all_named()

  pkg <- defer_(function(x)summary(x), summary)

  expect_true(is_execution_package(pkg))
  expect_equal(list_functions(pkg), 'entry')
  expect_equal(list_dependencies(pkg, c(base = 'summary')))
})


# --- execution --------------------------------------------------------

test_that("package can be executed", {
  f   <- mock(1, 2)
  pkg <- defer_(f)

  expect_equal(run_package(pkg, 1, 2), 1)
  expect_equal(run_package(pkg, 3, 4), 2)

  expect_no_calls(f, 2)
  expect_args(f, 1, 1, 2)
  expect_args(f, 2, 3, 4)
})


test_that("other function can be executed", {
  f   <- mock(1)
  pkg <- defer_(function(...){}, f = f)

  expect_equal(run_package(pkg, .fun = 'f', 1, 2), 1)
  expect_no_calls(f, 1)
  expect_args(f, 1, 1, 2)
})


test_that("named entry function can be executed", {
  f   <- mock(1)
  pkg <- defer_('f', f = f)

  expect_equal(run_package(pkg, 1, 2), 1)
  expect_no_calls(f, 1)
  expect_args(f, 1, 1, 2)
})


# --- complex execution ------------------------------------------------

test_that("functions can execute each other", {
  m <- mock(1)
  f <- function(...) g(...)

  # mock is passed under a different name to verify that f() doesn't reach
  # back to *this* environment
  pkg <- defer_(f, g = m)

  expect_equal(run_package(pkg, 1, 2), 1)
  expect_no_calls(m, 1)
  expect_args(m, 1, 1, 2)
})


# --- loading libraries ------------------------------------------------

test_that("libraries are loaded and set up", {
  # TODO libraries need to be either loaded in the same order as
  #      it was upon package creation or just symbols referred to in
  #      the package need to be present
})


