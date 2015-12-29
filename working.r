# a sample use case to test ongoing work

pkg <- package(function(x) {
  lm(Sepal.Length ~ Species, data = x)
})

res <- evaluate(pkg, x = iris, remote = 'opencpu://remote-host:8080')


where <- paste0(opencpu$url(), '/library/defer/R/opencpu_evaluate/json')
GET(where)


# another sample use case
library(dplyr)

iris %>%
  select(Sepal.Length, Species) %>%
  { lm(Sepal.Length ~ Species, data = .) } %>%
  remote('opencpu://remote-host:8080')
