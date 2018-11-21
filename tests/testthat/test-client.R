context("client")


test_that("rserve manager", {
  skip_if_not_installed('RSclient')
  skip_if_not_installed('Rserve')
  skip_on_appveyor()
  skip_on_travis()

  Rserve::Rserve(wait = FALSE, args = "--no-save --RS-enable-control --slave")
  Sys.sleep(0.2)

  rsc <- RSclient::RS.connect()
  on.exit(RSclient::RS.server.shutdown(rsc))

  manager <- defer:::RServeManager$new(4)
  results <- list()
  cont <- function(which) function(res) results[[which]] <<- res

  manager$add(function()1, cont("r1"))
  manager$add(function()2, cont("r2"))
  manager$add(function()3, cont("r3"))
  manager$add(function()4, cont("r4"))

  expect_silent(manager$wait_all())
  expect_length(results, 4)
  expect_named(results, c("r1", "r2", "r3", "r4"))
  expect_equal(as.numeric(results), 1:4)
})
