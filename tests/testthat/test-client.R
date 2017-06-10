context("client")


test_that("rserve manager", {
  manager <- defer:::RServerManager$new(4)

  expect_null(manager$result())
  manager$add("1", function()1)
  manager$add("2", function()2)
  manager$add("3", function()3)
  manager$add("4", function()4)

  expect_equal(manager$result(), list(id = "1", value = 1))
})

