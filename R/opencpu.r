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


#' @importFrom base64enc base64encode
serialize_object <- function (object) {
  rds_buffer <- rawConnection(raw(0), 'w')
  on.exit(close(rds_buffer))
  saveRDS(object, rds_buffer)
  base64encode(rawConnectionValue(rds_buffer))
}


#' @importFrom base64enc base64decode
deserialize_object <- function (string) {
  raw_buffer <- rawConnection(base64decode(string), 'r')
  on.exit(close(raw_buffer))
  readRDS(raw_buffer)
}
