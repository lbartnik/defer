#' @export
evaluate <- function (pkg, ..., remote, block = FALSE) {
  # TODO come up with syntax to specify the remote host
  # TODO the default remote engine is mcparallel
  if (missing(remote))
    remote <- 'local'
  ev <- get_evaluator(remote)

  handle <- evaluate_impl(ev, pkg, list(...))
  if (!block)
    return(handle)

  handle$collect(TRUE)$result()
}


#' @importFrom httr parse_url
#' @importFrom stringi stri_replace_first_fixed
get_evaluator <- function (remote) {
  if (remote == 'local')
    return(structure(list(), class = 'local'))

  if (parse_url(remote)$scheme == 'opencpu') {
    remote <- stri_replace_first_fixed(remote, 'opencpu', 'http')
    return(structure(remote, class = 'opencpu'))
  }

  stop('cannot recognize remote backend', call. = FALSE)
}


#' Evaluator factory.
evaluate_impl <- function (evaluator, package, arguments)
  UseMethod('evaluate_impl')


#' @importFrom parallel mcparallel
evaluate_impl.local <- function (evaluator, package, arguments) {
  # TODO add package dependencies to exec_env
  exec_env <- new.env(parent = globalenv())
  environment(package$fun) <- exec_env
  do.call(package$fun, arguments, envir = exec_env)
}
