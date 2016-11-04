context("package internals")

test_that("final environment can be found", {
  expect_true(is_library_dependency(summary))

  f <- function(){}
  environment(f) <- globalenv()
  expect_false(is_library_dependency(f))
})
