lm_package <- function () {
  package(function(x) {
    lm(Sepal.Length ~ Species, data = x)
  })
}

lm_local <- function (x) {
  lm(Sepal.Length ~ Species, data = x)
}
