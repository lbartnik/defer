context("extras")

test_that("function can be listed", {
  f <- function()1
  g <- function()2
  d <- defer_(function()f()+g(), f, g)

  expect_is(d, 'deferred')
  expect_setequal(names(extract_functions(d)), c('entry', 'f', 'g'))
})


test_that("library functions are not packaged but recorded", {
  d <- defer_(function(){}, summary = summary)

  expect_is(d, 'deferred')
  expect_equal(names(extract_functions(d)), 'entry')

  libs <- extract_dependencies(d)
  expect_named(libs, c('fun', 'pkg', 'ver'))
  expect_equal(libs$fun, 'summary')
  expect_equal(libs$pkg, 'base')
  expect_equal(libs$ver, as.character(packageVersion("base")))
})
