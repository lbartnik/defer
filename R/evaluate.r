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
  exec_env <- new.env(parent = globalenv())
  environment(package$fun) <- exec_env
  do.call(package$fun, arguments, envir = exec_env)
}


#' @importFrom base64enc base64encode
serialize_object <- function (object) {
  rds_buffer <- rawConnection(raw(0), 'w')
  on.exit(close(rds_buffer), add = TRUE)
  saveRDS(object, rds_buffer)
  base64encode(rawConnectionValue(rds_buffer))
}


#' @importFrom base64enc base64decode
deserialize_object <- function (string) {
  raw_vector <- base64decode(string)
  readRDS(rawConnection(raw_vector, 'r'))
}


#' Evaluate with OpenCPU.
#' @return serialized & encoded object and ...
#' @importFrom parallel mcparallel
#' @importFrom httr POST
evaluate_impl.opencpu <- function (ev, package, arguments) {
  where <- paste0(ev, "/library/defer/R/opencpu_evaluate/json")
  body  <- list(
    package   = paste0('"', serialize_object(package), '"'),
    arguments = paste0('"', serialize_object(arguments), '"')
  )

  # run & create handle
  # TODO replace some_name with hash of body
  result_handle <- mcparallel(POST(where, body = body), name = 'some_name', silent = TRUE)

  if (is_error(e <- result_handle))
    stop(as.character(e), call. = FALSE)

  # return an interactive handle
  OpenCPU_Handle$new(result_handle)
}


#' @importFrom parallel mccollect
#' @importFrom R6 R6Class
#' @importFrom httr content
OpenCPU_Handle <- R6::R6Class('OpenCPU_Handle',
  public = list(
    initialize = function (result_handle) {
      private$result_handle <- result_handle
    },
    print = function () {
      if (is.null(private$result_object))
        public$collect(FALSE)
      if (is.null(private$result_object)) {
        cat('results not available yet\n')
      }
      else {
        cat('results from a remote OpenCPU run:\n')
        print(private$result_object)
      }
      invisible(self)
    },
    collect = function (wait = FALSE) {
      result <- mccollect(private$result_handle, wait = wait)[[1]]
      if (!is.null(result)) {
        if (is_error(result))
          stop(as.character(result), call. = FALSE)
        # TODO check return code, MIME type, etc. of response
        private$result_object <- deserialize_object(content(result, 'text'))
      }
      return(self)
    },
    result = function () {
      return(private$result_object)
    }
  ),
  private = list(
    result_handle = NULL,
    result_object = NULL
  )
)


#' @export
#' @importFrom base64enc base64decode
opencpu_evaluate <- function (package, arguments) {
  package   <- deserialize_object(package)
  arguments <- deserialize_object(arguments)

  ev  <- get_evaluator('local')
  res <- evaluate_impl(ev, package, arguments)
  serialize_object(res)
}
