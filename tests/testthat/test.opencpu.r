context('opencpu')


test_that('OpenCPU evaluation works end-to-end', {
  # OpenCPU must be present & loaded
  skip_if_not_installed('opencpu')
  if (!require(opencpu)) skip('could not load OpenCPU')

  # start a new server
  expect_null(opencpu::opencpu$start())

  res <- evaluate(lm_package(), x = iris, block = TRUE,
                  remote = gsub('http', 'opencpu', opencpu::opencpu$url()))

  expect_equal(res, lm_local(iris))

  # stop the server
  expect_null(opencpu::opencpu$stop())
})


# tests the basic logic of client-side evaluator and the
# OpenCPU_Handle class
test_that('opencpu client evaluator works', {
  with_mock(
    `parallel::mcparallel` = function(x, ...)x,
    `parallel::mccollect`  = function(...) serialize_object(1:100),
    `httr::POST`           = function(x, body)body,
    `httr::content`        = function(x, ...)x,
    {
      handle <- evaluate_impl.opencpu('url', list(), list())
      handle$collect()
      expect_equal(handle$result(), 1:100)
    }
  )
})


test_that('opencpu server evaluator works', {
  res <- opencpu_evaluate(serialize_object(lm_package()),
                          serialize_object(list(x = iris)))
  expect_equal(deserialize_object(res), lm_local(iris))
})


test_that('serialization/deserialization works', {
  x <- rnorm(100)
  expect_equal(deserialize_object(serialize_object(x)), x)
})

