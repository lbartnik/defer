context("packaging")


test_that("package anonymous function", {
  pkg <- package(function(x) {
    summary(x)
  })
  res <- evaluate(pkg, iris)
  expect_equal(res, summary(iris))
})


test_that("package named function", {
  fun <- function(x) { summary(x) }
  pkg <- package(fun)
  res <- evaluate(pkg, iris)
  expect_equal(res, summary(iris))
})


