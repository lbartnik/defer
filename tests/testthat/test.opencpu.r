context('opencpu')


test_that('OpenCPU evaluation works', {
  # OpenCPU must be present & loaded
  skip_if_not_installed('opencpu')
  if (!require(opencpu)) skip('could not load OpenCPU')

  # start a new server
  opencpu::opencpu$start()
  remote <- gsub('http', 'opencpu', opencpu::opencpu$url())

  pkg <- package(function(x) {
    lm(Sepal.Length ~ Species, data = x)
  })

  res <- evaluate(pkg, x = iris[1:100,], remote = remote, block = TRUE)

  x <- iris[1:100,]
  expect_equal(res, lm(Sepal.Length ~ Species, data = x))

  # stop the server
  opencpu::opencpu$stop()
})
