context("dependencies")

# --- various types of functions ---------------------------------------

test_that("regular function can be packaged", {
  skip("not implemented yet")

  pkg <- package_(function(x)summary(x), .extract = TRUE)
  expect_true(is_execution_package(pkg))
  expect_equal(list_dependencies(pkg, c(base = 'summary')))

  ret <- local(run_package(pkg, iris), envir = globalenv())
  expect_equal(ret, summary(iris))
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
