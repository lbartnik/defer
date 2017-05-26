# a sample use case to test ongoing work

library(rlang)

f <- function(...)quos(...)

a <- 1
b <- 2
c <- 3

x <- f(a,b,c)
y <- c(x, list(f=f))

eval_tidy(x)
eval_tidy(y)


defer(entry) -> extracts, functions and variables
defer(entry, fun) -> extracts, functions and variables
defer(entry, var) -> -> extracts, functions and variables
defer(entry, fun, var) -> -> extracts, functions and variables

defer(entry, name)
defer(entry, name = value)
defer(entry, name = name2)

defer(entry, functions = c("name", "name2"))

args <- list(name = value, name2 = value2)
defer(entry, functions = args)

