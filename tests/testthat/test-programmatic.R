context("packaging")


# --- packaging --------------------------------------------------------

test_that("execution package is returned and function can be listed", {
  f   <- function(){}
  df <- defer_(f)
  expect_true(is_deferred(df))
  expect_equal(extract_functions(df), 'entry')
})


test_that("multiple functions can be packaged", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)
  df <- defer_(f, g = g)

  expect_true(is_deferred(df))
  expect_true(setequal(extract_functions(df), c('entry', 'g')))
})


test_that("only named functions are allowed", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)

  expect_error(defer_(f, g))
  expect_error(defer_(f, 1))
})


test_that("library functions are not packaged but recorded", {
  df <- defer_(function(){}, summary = summary)

  expect_true(is_deferred(df))
  expect_equal(extract_functions(df), 'entry')
  expect_equal(extract_dependencies(df), c(base = 'summary'))
})


test_that("%>% is recognized as regular dependency", {
  skip_if_not_installed("magrittr")

  df <- defer_(function(){}, f = . %>% summary(.))

  expect_true(is_deferred(df))
  expect_true(setequal(extract_functions(df), c('entry', 'f')))
})

# --- passing functions through functions ---------------------------------

test_that("functions can be passed by functions", {
  df <- defer_(function(x)f(x), functions = list(f = function(y)summary(y)))

  expect_true(is_deferred(df))
  expect_true(setequal(extract_functions(df), c('entry', 'f')))
  expect_equal(run_deferred(df, iris), summary(iris))
})


test_that("handling errors in functions", {
  # unnamed
  expect_error(defer_(function(){}, functions = list(function(){})),
               "all elements in `functions` must be named")

  # not a function
  expect_error(defer_(function(){}, functions = list(f = 1)),
               "only function objects can be passed via `functions`")

  # name conflict
  expect_error(defer_(function(){}, f = function(){}, functions = list(f = function(){})),
               "names in ... and `functions` cannot overlap")
})


# --- unnamed library hints --------------------------------------------

test_that("unnamed symbols can be passed in ...", {
  skip("this functionality is not yet implemented")

  # TODO to implement make a change in make_all_named()

  df <- defer_(function(x)summary(x), summary)

  expect_true(is_deferred(df))
  expect_equal(extract_functions(df), 'entry')
  expect_equal(extract_dependencies(df, c(base = 'summary')))
})


# --- execution --------------------------------------------------------

test_that("package can be executed", {
  f   <- mock(1, 2)
  df <- defer_(f)

  expect_equal(run_deferred(df, 1, 2), 1)
  expect_equal(run_deferred(df, 3, 4), 2)

  expect_called(f, 2)
  expect_args(f, 1, 1, 2)
  expect_args(f, 2, 3, 4)
})


# --- complex execution ------------------------------------------------

test_that("functions can execute each other", {
  m <- mock(1)
  f <- function(...) g(...)

  # mock is passed under a different name to verify that f() doesn't reach
  # back to *this* environment
  df <- defer_(f, g = m)

  expect_equal(run_deferred(df, 1, 2), 1)
  expect_called(m, 1)
  expect_args(m, 1, 1, 2)
})


# --- loading libraries ------------------------------------------------

test_that("libraries are loaded and set up", {
  # TODO libraries need to be either loaded in the same order as
  #      it was upon package creation or just symbols referred to in
  #      the package need to be present
})


