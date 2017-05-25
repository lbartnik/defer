#' @export
defer <- function (entry, ..., .dots, .extract = TRUE)
{
  dots <- quos(...)
  if (!missing(.dots)) {
    stopifnot(is.list(.dots))
    dots <- c(dots, .dots)
  }

  defer_(entry, .dots = dots, .extract = .extract)
}


#' Defer function execution - create an execution package.
#'
#' @param entry Entry-point function or a function name.
#' @param ... List of dependencies, functions and variables.
#' @param functions A list of functions.
#' @param variables A list of variables.
#' @param .extract Whether to analyze functions and extract dependencies
#'        from their bodies.
#' 
#' @return A deferred function object.
#'
#' @export
#' @rdname package
#' @seealso \code{\link[defer]{execute}}
#'
#' @importFrom rlang quos eval_tidy caller_env
#'
defer_ <- function (entry, ..., .dots = list(), .extract = FALSE)
{
  # TODO should library-function names be extracted even in the programmer's API?

  # entry must be a regular function
  stopifnot(is.function(entry))
  stopifnot(is.list(.dots))
  
  # capture expressions with quos() and make sure all element are named
  dots  <- quos(...)
  dots  <- tryCatch(eval_tidy(make_all_named(dots)), error = function(e) stop(
    "some arguments passed in ... are not named and names cannot be auto-generated", call. = FALSE))
  .dots <- tryCatch(eval_tidy(make_all_named(.dots)), error = function(e) stop(
    "some elements in `.dots` are not named and names cannot be auto-generated", call. = FALSE))

  # no overlaps are allowed
  if (length(intersect(names(dots), names(.dots)))) {
    stop("names in ... and `.dots` cannot overlap", call. = FALSE)
  }
  if ('entry' %in% names(.dots)) {
    stop('cannot use `entry` as a name in `.dots`', call. = FALSE)
  }

  # --- put all dependencies together and then extract each category one by one
  dots <- c(dots, .dots)
  dots$entry <- entry
  eval_env <- caller_env()
  
  processor <- DependencyProcessor$new(dots, eval_env)
  processor$run(.extract)

  # --- prepare and return the deferred execution function object
  
  executor <- defer:::executor
  exec_env <- environment(executor) <- new.env(parent = eval_env)
  
  exec_env$function_deps <- processor$function_deps
  exec_env$library_deps  <- processor$library_deps
  exec_env$variables     <- processor$variables
  
  class(executor) <- c("deferred", "function")
  
  executor
}


#' @importFrom rlang UQE
make_all_named <- function (args)
{
  is_double_colon <- function (x) is.call(x) && identical(x[[1]], bquote(`::`))
  into_name       <- function (x) {
    e <- UQE(x)
    if (is.name(e)) return(as.character(e))
    if (is_double_colon(e)) return(deparse(e))
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
  isNamespace(environment(x))
}



library(R6)

#' @importFrom dplyr data_frame
#' @importFrom rlang caller_env
DependencyProcessor<- R6::R6Class("DependencyProcessor",
  public = list(
    library_deps = NA,
    function_deps = NA,
    variables = NA,
    
    initialize = function (deps, caller_env) {
      private$deps <- deps
      private$caller_env <- caller_env
    },
    
    # 1. extract regular functions
    # 2. extract variables
    # 3. extract library functions
    # 4. nothing else should be left
    #
    run = function (extract = FALSE)
    {
      private$extract_library_deps()
      private$extract_regular_functions()
      private$extract_variables()

      if (length(private$deps)) {
        stop('unprocessed dependencies left', call. = FALSE)
      }
    }
  ),
  private = list(
    deps = NA,
    caller_env = NA,
    
    extract_library_deps = function () {
      i <- vapply(private$deps, is_library_dependency, logical(1))
      deps <- private$deps[i]
      private$deps <- private$deps[!i]
      
      # turn dots into names + pkg names
      if (!length(deps)) return()
  
      pkg_names <- vapply(deps, function(x) environmentName(environment(x)), character(1))
      fun_names <- names(deps)
      versions  <- seq_along(fun_names) # TODO extrat package version
      
      self$library_deps <- data_frame(fun = fun_names, pkg = pkg_names, ver = versions)
    },
    
    # Extracts regular functions.
    #
    extract_regular_functions = function () {
      i <- vapply(private$deps, is.function, logical(1))
      deps <- private$deps[i]
      private$deps <- private$deps[!i]
      
      # remove environment from a function unless it's a closure
      self$function_deps <- lapply(deps, function (f) {
        if (identical(environment(f), private$caller_env)) {
          environment(f) <- emptyenv()
        }
        f
      })
    },
    
    extract_variables = function () {
      i <- vapply(private$deps, function(x) is.vector(x) || is.list(x), logical(1))
      self$variables <- private$deps[i]
      private$deps <- private$deps[!i]
    }
  )
)



#' @description \code{is_deferred} verifies if the given object
#' is an execution package.
#'
#' @param x Object to be tested.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @rdname package
#'
is_deferred <- function (x) inherits(x, 'deferred')


#' @description \code{list_functions} returns a \code{character} vector
#' of names of functions packaged in \code{pkg}.
#'
#' @param pkg An execution package object.
#' @return A vector of function names.
#'
#' @export
#' @rdname package
#'
extract_functions <- function (df)
{
  stopifnot(is_deferred(df))
  ee <- environment(df)
  return(names(ee$function_deps))
}



#' \code{list_dependencies} returns a \code{character} vector of
#' function names that come from other R packages. Each function
#' name (vector value) has a corresponding package name set in
#' \code{\link{names}}.
#'
#' @return \code{list_dependencies} returns a named vector of
#' functions that belong to other R packages.
#'
#' @export
#' @rdname package
#'
extract_dependencies <- function (df)
{
  stopifnot(is_deferred(df))
  ee <- environment(df)
  return(ee$library_deps)
}

