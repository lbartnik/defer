#' @export
package <- function (fun, ...) {
  # TODO add the ... argument to help resolve symbols referred in 'fun'

  # TODO what to do with the environment of the input function?
  #      handle various types of fun:
  #       1. regular function
  #       2. closure
  #       3. a pipe object

  environment(fun) <- emptyenv()

  # TODO later on we will add dependencies: user-defined objects (functions)
  #      and library symbols
  structure(list(fun = fun), class = 'deferred')
}


#' @export
#' @importFrom lazyeval lazy_eval
package_ <- function (fun, dots) {
  objs <- lazy_eval(dots)
  structure(list(fun = fun, objs = objs), class = 'deferred')
}
