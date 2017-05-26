context("programmatic")


# --- packaging --------------------------------------------------------

test_that("execution package is returned and function can be listed", {
  f   <- function(){}
  df <- defer_(f)
  expect_is(df, 'deferred')
  expect_equal(extract_functions(df), 'entry')
})


test_that("multiple functions can be packaged", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)
  df <- defer_(f, g = g)

  expect_is(df, 'deferred')
  expect_setequal(extract_functions(df), c('entry', 'g'))
})


test_that("functions can be referred to by name", {
  f <- function(x)x*x
  g <- function(a)sqrt(a)

  df <- defer_(f, g)
  expect_is(df, 'deferred')
  expect_setequal(extract_functions(df), c('entry', 'g'))
})


test_that("library functions are not packaged but recorded", {
  df <- defer_(function(){}, summary = summary)

  expect_is(df, 'deferred')
  expect_equal(extract_functions(df), 'entry')

  libs <- extract_dependencies(df)
  expect_named(libs, c('fun', 'pkg', 'ver'))
  expect_equal(libs$fun, 'summary')
  expect_equal(libs$pkg, 'base')
  expect_equal(libs$ver, 1)
})


test_that("%>% is recognized as regular dependency", {
  skip_if_not_installed("magrittr")

  df <- defer_(function(){}, f = . %>% summary(.))

  expect_is(df, 'deferred')
  expect_setequal(extract_functions(df), c('entry', 'f'))
})

# --- passing functions through functions ---------------------------------

test_that("functions can be passed by functions", {
  df <- defer_(function(x)f(x), .dots = list(f = function(y)summary(y)))

  expect_is(df, 'deferred')
  expect_setequal(extract_functions(df), c('entry', 'f'))
  expect_equal(run_deferred(df, iris), summary(iris))
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


# --- unnamed library hints --------------------------------------------

test_that("unnamed symbols can be passed in ...", {

  df <- defer_(function(x)summary(x), summary)

  expect_is(df, 'deferred')
  expect_equal(extract_functions(df), 'entry')
  
  deps <- extract_dependencies(df)
  expect_equal(deps$fun, "summary")
  expect_equal(deps$pkg, "base")
  expect_equal(deps$ver, 1)
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


