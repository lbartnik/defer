#' Defer function execution.
#'
#' Both \code{defer} and \code{defer_} create an execution package
#' (wrapper) for any user-provided function.
#'
#' \code{defer} is intended for interactive use - it assumes that
#' dependencies should be extracted (\code{.extract} defaults to
#' \code{TRUE}).
#'
#' @param entry Entry-point function.
#' @param ... List of dependencies, functions and variables.
#' @param .dots A list of functions or quosures (see \code{\link[rlang]{quos}}).
#' @param .extract Whether to analyze functions and extract dependencies
#'        from their code.
#'
#' @return A \code{deferred} function object.
#'
#' @export
#' @rdname defer
#'
#' @import rlang
#'
defer <- function (entry, ..., .dots, .extract = TRUE)
{
  dots <- quos(...)
  if (!missing(.dots)) {
    stopifnot(is.list(.dots))
    dots <- c(dots, .dots)
  }

  .caller_env <- caller_env()
  defer_(entry, .dots = dots, .extract = .extract, .caller_env = .caller_env, .verbosity = 1)
}



#' @description \code{defer_} is intended for non-interactive use. It
#' provides an interface very similar to \code{defer} but by default
#' turns off discovering dependencies (\code{.extract} is \code{FALSE}).
#'
#' @param .caller_env The environment where \code{defer_()} is supposed to
#'        assume the call was made and the wrapper is returned to. Its
#'        value is important when \code{.extract} is set to \code{TRUE},
#'        and it is used in the interactive version, \code{defer()}, which
#'        passes its own \code{caller_env()} to \code{defer_()}.
#'
#' @param .verbosity Accepts values 0, 1 and 2. 0 means quiet, 1 and 2
#'        result in additional output for the user. Set to \code{1} when
#'        in interactive mode, that is, when called from \code{defer()}.
#'
#' @export
#' @rdname defer
#' @import rlang
#'
defer_ <- function (entry, ..., .dots = list(), .extract = FALSE, .caller_env = caller_env(), .verbosity = 0)
{
  # TODO should library-function names be extracted even in the programmer's API?

  # entry must be a regular function
  stopifnot(is.function(entry))
  stopifnot(is.list(.dots))
  stopifnot(.verbosity %in% 0:2)

  # capture expressions with quos() and make sure all element are named
  dots  <- quos(...)

  dots  <- tryCatch(lapply(make_all_named(dots), eval_tidy), error = function(e) stop(
    "some arguments passed in ... are not named and names cannot be auto-generated", call. = FALSE))
  .dots <- tryCatch(lapply(make_all_named(.dots), eval_tidy), error = function(e) stop(
    "some elements in `.dots` are not named and names cannot be auto-generated", call. = FALSE))

  # no overlaps are allowed
  if (length(intersect(names(dots), names(.dots)))) {
    stop("names in ... and `.dots` cannot overlap", call. = FALSE)
  }
  if ('entry' %in% names(.dots)) {
    stop('cannot use `entry` as a name in `.dots`', call. = FALSE)
  }

  # --- put all dependencies together and then extract each category one by one
  deps <- c(dots, .dots, list(entry = entry))

  processor <- DependencyProcessor$new(deps, .caller_env)
  processor$run(.extract, .verbosity)

  # --- prepare and return the deferred execution function object

  executor <- executor
  exec_env <- environment(executor) <- new.env(parent = globalenv())

  exec_env$function_deps <- processor$function_deps
  exec_env$library_deps  <- processor$library_deps
  exec_env$variables     <- processor$variables
  exec_env$arguments     <- list()

  formals(executor) <- formals(deps$entry)
  if (match("...", names(formals(executor)), 0) == 0) {
    formals(executor) <- c(formals(executor), alist(...=))
  }

  class(executor) <- c("deferred", "function")

  executor
}


#' @rdname defer
#' @export
#' @import rlang
caller_env <- caller_env


#' @description \code{is_deferred} verifies if the given object
#' is a \code{deferred} function wrapper.
#'
#' @param x Object to be tested.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @rdname defer
#'
is_deferred <- function (x) inherits(x, 'deferred')



#' @description Pass a value in place of an argument. This function will
#' modify the input object.
#'
#' @param deferred A \code{deferred} function wrapper.
#' @param ... Name-value pairs, where name is the name of an argument to
#'        the \code{entry} function.
#' @return Modified \code{deferred} function wrapper. Note that the
#'         original function object is also modified.
#'
#' @export
#' @rdname defer
#' @importFrom rlang env_clone
#'
#' @examples
#' d <- defer(function(a, b, c) return(a+b+c))
#' augment(d, a = 1, b = 2, c = 3)
#' d()
#' #> 6
#'
augment <- function (deferred, ...)
{
  args <- list(...)
  if (any(!nchar(names(args)))) {
    stop("all arguments must be named", call. = FALSE)
  }

  i <- !(names(args) %in% names(formals(deferred)))
  if (any(i)) {
    stop("following names are not among arguments of `deferred`: ",
         paste(names(args)[i], collapse = ", "), call. = FALSE)
  }

  cur <- environment(deferred)$arguments
  i <- (names(args) %in% names(cur))
  if (any(i)) {
    warning("following arguments are already augmented and will be reset: ",
            paste(names(args)[i], collapse = ", "), call. = FALSE)
  }

  for (name in names(args)) {
    cur[[name]] <- args[[name]]
  }

  # create a copy of deferred and assign arguments
  new_deferred <- deferred
  environment(new_deferred) <- env_clone(environment(deferred))
  environment(new_deferred)$arguments <- cur

  return(new_deferred)
}



# ---------------------------------------------------------------------

#' @importFrom rlang UQE
make_all_named <- function (args)
{
  is_double_colon <- function (x) is.call(x) && identical(x[[1]], bquote(`::`))
  into_name       <- function (x) {
    e <- UQE(x)
    if (is.name(e)) return(as.character(e))
    if (is_double_colon(e)) return(deparse(e[[3]]))
    ""
  }

  if (is.null(names(args)) || !length(names(args))) {
    names(args) <- rep("", length(args))
  }

  empty <- !nchar(names(args))
  if (!any(empty)) return(args)

  new_names <- vapply(args[empty], into_name, character(1))

  if (any(!nchar(new_names))) {
    stop("some objects are not named and names cannot be auto-generated",
         call. = FALSE)
  }

  names(args)[empty] <- new_names
  args
}


is_library_dependency <- function (x)
{
  is.function(x) && isNamespace(environment(x))
}

is_closure <- function (x, caller_env)
{
  !identical(environment(x), caller_env) &&
    !identical(environment(x), globalenv())
}

is_assignment <- function (x) identical(x[[1]], bquote(`<-`))


library(R6)

#' @importFrom rlang caller_env
#' @importFrom R6 R6Class
DependencyProcessor<- R6::R6Class("DependencyProcessor",
  public = list(
    library_deps  = data.frame(pkg = character(), fun = character(), ver = character(),
                               stringsAsFactors = FALSE),
    function_deps = list(),
    variables = list(),

    initialize = function (deps, caller_env) {
      private$deps <- deps
      private$caller_env <- caller_env
    },

    # 1. extract regular functions
    # 2. extract variables
    # 3. extract library functions
    # 4. nothing else should be left
    #
    run = function (extract = FALSE, verbosity = 0)
    {
      private$extract   <- extract
      private$verbosity <- verbosity
      private$process()
      private$summary()
    }
  ),
  private = list(
    deps       = list(),
    processed  = list(),
    caller_env = NA,
    extract    = FALSE,
    verbosity  = 0,

    process = function () {
      while (length(private$deps)) {
        name    <- names(private$deps)[1]
        current <- private$deps[[1]]
        private$deps <- private$deps[-1]

        if (is_library_dependency(current)) {
          private$process_library(name, current)
        }
        else if (is.function(current)) {
          private$process_function(name, current)
        }
        else if (is.vector(current) || is.list(current)) {
          private$process_variable(name, current)
        }
        else {
          stop("cannot process")
        }
      }
    },

    process_library = function (name, fun) {
      pkg_name <- environmentName(environment(fun))
      pkg_ver  <- as.character(getNamespaceVersion(pkg_name))
      new_dep  <- data.frame(fun = name, pkg = pkg_name, ver = pkg_ver, stringsAsFactors = FALSE)

      private$verbose("Adding library call: ", pkg_name, '::', name)
      self$library_deps <- rbind(self$library_deps, new_dep)
    },

    # Extracts regular functions.
    # remove environment from a function unless it's a closure
    #
    process_function = function (name, fun) {
      if (!is_closure(fun, private$caller_env)) {
        environment(fun) <- emptyenv()
      }

      private$verbose("Adding function: ", name)
      self$function_deps[[name]] <- fun

      if (isTRUE(private$extract)) {
        private$verbose("Processing function: ", name)
        private$process_body(body(fun))
      }
    },

    process_variable = function (name, value) {
      private$verbose("Adding variable: ", name)
      self$variables[[name]] <- value
    },

    # https://stackoverflow.com/questions/14276728/finding-the-names-of-all-functions-in-an-r-expression/14295659#14295659
    process_body = function (x) {
      recurse <- function(x) sort(unique(as.character(unlist(lapply(x, private$process_body)))))

      already_found <- function (x) (f_name %in% c(names(self$function_deps), self$library_deps$fun, names(self$deps)))

      if (is.name(x)) {
        v_name <- as.character(x)
        if (!nchar(v_name) || !exists(v_name, envir = private$caller_env, inherits = TRUE)) return()

        candidate <- get(v_name, envir = private$caller_env)
        if (!is.numeric(candidate) && !is.character(candidate)) return()

        self$variables[[v_name]] <- get(v_name, envir = private$caller_env)
        private$verbose("  - adding candidate variable: ", v_name)
      }
      else if (is_assignment(x)) {
        return(recurse(x[-(1:2)]))
      }
      else if (is.call(x)) {
        f_name <- as.character(x[[1]])
        if (already_found(f_name)) {
          return(recurse(x[-1]))
        }

        if (exists(f_name, envir = private$caller_env, mode = 'function', inherits = TRUE)) {
          f_obj <- get(f_name, envir = private$caller_env)
          if (!is.primitive(f_obj)) {
            private$deps[[f_name]] <- f_obj
            private$verbose("  - adding candidate function: ", f_name)
          }
        }

        return(recurse(x[-1]))
      }

      if (is.recursive(x)) recurse(x)
    },

    verbose = function (...) {
      if (identical(private$verbosity, 2)) {
        message(paste(..., collapse = " ", sep = ""))
      }
    },

    summary = function () {
      if (identical(private$verbosity, 1) || identical(private$verbosity, 2)) {
        formatted <- format_deferred(self)
        if (nchar(formatted) > 0) {
          message("Found ", formatted)
        }
      }
    }
  )
)






