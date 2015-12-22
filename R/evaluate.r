#' @export
#' @importFrom parallel mcparallel
evaluate <- function (pkg, ..., remote) {
  # TODO come up with syntax to specify the remote host
  # TODO the default remote engine is mcparallel
  if (missing(remote)) {
    remote <- mcparallel
  }

  exec_env <- new.env(parent = globalenv())
  environment(pkg$fun) <- exec_env

  do.call(pkg$fun, list(...), envir = exec_env)
}
