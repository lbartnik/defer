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


test_that("%>% can be packaged", {
  skip("not implemented yet")
  skip_if_not_installed("magrittr")
  
  # TODO make sure dependencies are extracted
  pkg <- package_(. %>% summary(.), .extract = TRUE)
  expect_true(is_execution_package(pkg))
  expect_equal(list_dependencies(pkg, c(base = 'summary')))
  
  ret <- local(run_package(pkg, iris), envir = globalenv())
  expect_equal(ret, summary(iris))
})

