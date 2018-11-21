
# --- opencpu ----------------------------------------------------------

#' Runs a deferred function in a OpenCPU R session.
#'
#' @param ocpu_url URL of the OpenCPU server.
#' @param deferred A deferred function wrapper.
#' @param ... Parameters for \code{deferred}, must be named.
#'
#' @importFrom utils head tail
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ocpu_url <- 'https://cloud.opencpu.org/ocpu/'
#' deferred <- defer(function(x, y)paste(x, y))
#'
#' opencpu(ocpu_url, deferred, x = 1, y = 2)
#' }
#'
opencpu <- function (ocpu_url, deferred, ...)
{
  if (!requireNamespace('httr') || !requireNamespace('jsonlite')) {
    stop('httr and jsonlite packages are required to ",
         "run a deferred function via opencpu', call. = FALSE)
  }

  # augment
  args <- list(...)
  if (length(args)) {
    deferred <- do.call(augment, c(list(deferred), make_all_named(args)))
  }

  # prepare the local script
  code <- deparse(body(remote_function))
  stopifnot(identical(head(code, 1), "{"),
            identical(tail(code, 1), "}"))

  # base64string is the argument of that function
  base64string <- jsonlite::base64_enc(serialize(deferred, NULL))
  code <- paste0("base64string <- '", base64string, "'\n",
                 paste(code[seq(2, length(code)-1)], collapse = "\n"))

  local_temp_path <- tempfile(fileext = ".R")
  cat(code, file = local_temp_path)

  # make the call
  url <- paste0(ocpu_url, "/library/base/R/source/json")
  http_result <- httr::POST(url,
                            body = list(file = httr::upload_file(local_temp_path)))

  result_value <- httr::content(http_result, 'text')
  result_value <- jsonlite::fromJSON(result_value)
  stopifnot("value" %in% names(result_value))

  unserialize(jsonlite::base64_dec(result_value$value))
}


#' Deserialize a base64-encoded string and run the deferred function.
#'
#' The body of this function is used in \code{\link{opencpu}}.
#' \code{jsonlite} is a dependency of opencpu so for sure it's going
#' to be there.
#'
#' This function exists because it's easier to write and test it this
#' way. However, it is never used directly, but rather deparsed and
#' sent to a remote server as text.
#'
#' @param base64string A serialized, base64-encoded deferred function
#'        wrapper.
#' @return A serialized, base64-encoded result of running the deferred
#'         wrapper.
#'
remote_function <- function (base64string)
{
  stopifnot(requireNamespace('jsonlite', quietly = TRUE))
  deferred <- unserialize(jsonlite::base64_dec(base64string))
  ans <- tryCatch({
    deferred()
  }, error = function(e)e)
  jsonlite::base64_enc(serialize(ans, NULL))
}


# --- dplyr ------------------------------------------------------------



#' @rdname dplyr
#'
#' @importFrom rlang caller_env
#' @export
#'
parallelize <- function (.data)
{
  class(.data) <- c("parallel", class(.data))
  attr(.data, "deferred_env") <- globalenv()
  .data
}


#' Run grouped \code{do} in parallel.
#'
#' Injects execution of \code{\link[dplyr]{do}} and if \code{.data} is
#' groupped, executes the operation in parallel.
#'
#' @param .data data object transformed with [parallelize]
#' @param ... expressions to apply to each group; see [dplyr::do]
#' @param env the environment in which functions should be evaluated; see [dplyr::do]
#' @param .dots pair/values of expressions coercible to lazy objects; see [dplyr::do]
#'
#' @importFrom rlang quos
#'
#' @rdname dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' iris %>%
#'   group_by(Species) %>%
#'   parallelize %>%
#'   do({
#'     # something that's better run in paralle
#'     data.frame(result = 1)
#'   })
#' }
#'
do_.parallel <- function (.data, ..., env = parent.frame(), .dots = list())
{
  stopifnot(requireNamespace('dplyr'))
  stopifnot(is.environment(attr(.data, 'deferred_env')))

  args <- c(quos(...), .dots)
  index <- attr(.data, "indices")
  n <- length(index)

  # if not groupped fall back on dplyr
  if (!inherits(.data, "grouped_df") || n == 0) {
    class(.data) <- setdiff(class(.data), "parallel")
    dplyr::do_(.data, env = env, .dots = args)
  }

  # follows dplyr:::do_.groupped_df
  group_data <- dplyr::ungroup(.data)
  labels <- attr(.data, "labels")
  m <- length(args)

  # turn expressions into functions that can be wrapped
  wrappers <- lapply(args, function (arg) {
    f <- function(.) {}
    environment(f) <- globalenv()
    body(f) <- arg$expr
    defer_(f, .caller_env = globalenv(), .extract = TRUE, .verbosity = 0)
  })
  names(wrappers) <- names(args)

  # run the job
  cat("parallel\n")
  manager <- RServeManager$new(12, FALSE)
  create_continuation <- function (i, j) {
    .i <- i
    .j <- j
    function(value) out[[.j]][.i] <<- list(value)
  }

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- dplyr::progress_estimated(n * m, min_time = 2)
  for (`_i` in seq_len(n)) {
    this_group <- group_data[index[[`_i`]] + 1L, , drop = FALSE]
    for (j in seq_len(m)) {
      w <- augment(wrappers[[j]], . = this_group)
      manager$add(w, create_continuation(`_i`, j), TRUE)
      p$tick()$print()
    }
  }

  manager$wait_all()

  stop('broken')
#  named <- dplyr:::named_args(args)
#  if (!named) {
#    dplyr:::label_output_dataframe(labels, out, groups(.data))
#  }
#  else {
#    dplyr:::label_output_list(labels, out, groups(.data))
#  }
}


library(R6)

#' @import R6
RServeManager <- R6::R6Class("RServeManager",
  public = list(

    initialize = function (n = 1, .debug = FALSE) {
      requireNamespace('RSclient', quitely = TRUE)

      private$pool <- lapply(seq(n), function(i) {
        list(state = "idle", connection = RSclient::RS.connect(), task = list())
      })

      private$debugging <- isTRUE(.debug)
    },

    add = function (fun, cnt = print, .block = FALSE) {
      private$debug("add()")
      task <- list(fun = fun, cnt = cnt)

      while (isTRUE(.block) && !length(private$which_idle())) {
        private$collect()
      }

      private$awaiting <- c(private$awaiting, list(task))
      if (!private$assign()) {
        private$collect()
      }

      invisible()
    },

    wait_all = function () {
      private$debug("wait_all()")
      while (length(private$which_idle()) != length(private$pool)) {
        Sys.sleep(.1)
        private$collect()
        private$assign()
      }
    }
  ), # public

  private = list(
    pool = list(),
    awaiting = list(),
    completed = list(),
    debugging = FALSE,

    assign = function () {
      private$debug("assign")
      fill_idle <- function (i) {
        if (!length(private$awaiting)) return(NULL)

        private$pool[[i]]$state <- "busy"
        private$pool[[i]]$task <- private$awaiting[[1]]
        private$awaiting <- private$awaiting[-1]

        private$debug("sending task")
        rs <- private$pool[[i]]

        RSclient::RS.assign(rs$connection, "fun", rs$task$fun, wait = FALSE)
        RSclient::RS.eval(rs$connection, fun(), wait = FALSE)
      }
      length(lapply(private$which_idle(), fill_idle)) > 0
    },

    collect = function () {
      private$debug("collect")
      ans <- lapply(private$pool, function (rs) {
        RSclient::RS.collect(rs$connection, timeout = 0, detail = TRUE)
      })
      inull <- vapply(ans, function (x) is.null(x$value), logical(1))

      private$debug("results collected: ", sum(!inull))

      for (i in seq_along(private$pool)[!inull]) {
        task <- private$pool[[i]]$task
        task$result <- ans[[i]]$value
        private$debug("running task continuation")
        task$cnt(task$result)

        private$pool[[i]]$task  <- list()
        private$pool[[i]]$state <- "idle"
      }
    },

    which_idle = function () {
      i <- vapply(private$pool, function (rs) identical(rs$state, "idle"), logical(1))
      seq_along(private$pool)[i]
    },

    debug = function (...) {
      if (isTRUE(private$debugging)) {
        cat("Debug: ", paste(...), "\n")
      }
    }
  ) # private
)
