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
defer_ <- function (entry, ..., .dots, .extract = FALSE)
{
  # TODO should library-function names be extracted even in the programmer's API?

  # entry must be a regular function
  stopifnot(is.function(entry))
  if (!missing(.dots)) stopifnot(is.list(.dots))
  
  # capture expressions with quos() and make sure all element are named
  dots <- quos(...)
  dots <- eval_tidy(make_all_named(dots))
  .dots <- eval_tidy(make_all_named(.dots))

  # no overlaps are allowed
  if (length(intersect(names(dots), names(.dots)))) {
    stop("names in ... and `.dots` cannot overlap", call. = FALSE)
  }
  if ('entry' %in% names(.dots)) {
    stop('cannot use the name `entry` among `.dots`', call. = FALSE)
  }

  # --- put all dependencies together and then extract each category one by one
  dots <- c(dots, .dots)
  dots$entry <- entry

  # split functions and library dependencies
  i <- vapply(dots, is_library_dependency, logical(1))
  library_deps <- dots[i]
  dots <- dots[!i]

  # turn dots into names + pkg names
  if (length(dots)) {
    package_names <- vapply(library_deps, function(x) environmentName(environment(x)),
                       character(1))
    library_deps <- names(library_deps)
    names(library_deps) <- package_names
  }
  else {
    library_deps <- character()
  }

  # extract regular functions
  i <- vapply(dots, is.function, logical(1))
  function_deps <- dots[i]
  dots <- dots[!i]
  
  # remove environment from a function unless it's a closure
  eval_env <- caller_env()
  function_deps <- lapply(function_deps, function (f) {
    if (identical(environment(f), eval_env)) {
      environment(f) <- emptyenv()
    }
    f
  })
  
  # there should be nothing left
  if (length(dots)) {
    stop('unprocessed dependencies left', call. = FALSE)
  }

  # --- prepare and return the deferred execution function object
  
  executor <- defer:::executor
  exec_env <- environment(executor) <- new.env(parent = eval_env)
  
  exec_env$function_deps <- function_deps
  exec_env$library_deps  <- library_deps
  
  class(executor) <- c("deferred", "function")
  
  executor
}


#' @importFrom rlang UQE
make_all_named <- function (args)
{
  empty <- !nchar(names(args))
  if (!any(empty)) return(args)

  into_name <- function (x) {
    e <- UQE(x)
    if (is.name(e)) return(as.character(e))
    if (is_double_colon(e)) return(deparse(e))
    ""
  }

  new_names <- vapply(args[empty], into_name, character(1))

  if (any(!nchar(new_names))) {
    stop("some objects are not named and names cannot be auto-generated",
         call. = FALSE)
  }

  names(args)[empty] <- new_names
  args
}

is_double_colon <- function (x)
{
  is.call(x) && identical(x[[1]], bquote(`::`))
}


is_all_functions <- function (objs)
{
  all(vapply(objs, is.function, logical(1)))
}


is_all_named <- function (objs)
{
  (length(objs) == 0) ||
    (!is.null(names(objs)) && all(names(objs) != ""))
}


is_library_dependency <- function (x)
{
  isNamespace(environment(x))
}



library(R6)
DependencyProcessor<- R6::R6Class("DependencyProcessor",
  public = list(
    dots = NA,
    initialize = function (dots) {
      self$dots <- dots
    },
    process = function (extract = FALSE) {
      # 1. extract regular functions
      # 2. extract variables
      # 3. extract library functions
      # 4. nothing else should be left
      
      private$extract_library_deps()
    }
  ),
  private = list(
    library_deps = NA,
    extract_library_deps <- function () {
      i <- vapply(self$dots, is_library_dependency, logical(1))
      library_deps <- self$dots[i]
      self$dots <- self$dots[!i]
      
      # turn dots into names + pkg names
      if (!length(library_deps)) return()
  
      pkg_names <- vapply(library_deps, function(x) environmentName(environment(x)), character(1))
      fun_names <- names(library_deps)
      versions  <- seq_along(fun_names) # TODO extrat package version
      
      self$library_deps <- data_frame(fun = fun_names, pkg = pkg_names, ver = verions)
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

