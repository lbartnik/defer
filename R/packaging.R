
#' Defer function execution - create an execution package.
#'
#' @param entry Entry-point function or a function name.
#' @param ... List of dependencies, functions and variables.
#' @param functions A list of functions.
#' @param variables A list of variables.
#' @return A deferred function object.
#'
#' @export
#' @rdname package
#' @seealso \code{\link[defer]{execute}}
#'
defer_ <- function (entry, ..., functions = list(), variables = list())
{
  # TODO should library-function names be extracted even in the programmer's API?

  # entry must be a regular function
  stopifnot(is.function(entry))
  stopifnot(is.list(functions), is.list(variables))
  
  if (length(variables)) {
    stop("`variables` are not supported yet", call. = FALSE)
  }
  
  # do not use list(...) because functions might be pointed by
  # names, e.g. defer(f, summary); in that case we first extract
  # the name (with make_all_named), and later the package name
  ellipsis <- eval(substitute(alist(...)))
  ellipsis <- make_all_named(ellipsis)
  
  # now extract the actual object
  eval_env <- parent.frame()
  ellipsis <- lapply(ellipsis, function(x)eval(x, envir = eval_env))

  if ('entry' %in% names(ellipsis)) {
    stop('cannot declare the `entry` function among ...', call. = FALSE)
  }
  else {
    ellipsis$entry <- entry
  }
    
  # prepare `functions`; only actual functions are allowed
  if (length(functions)) {
    if (!is_all_functions(functions)) {
      stop("only function objects can be passed via `functions`", call. = FALSE)
    }
    if (!is_all_named(functions)) {
      stop("all elements in `functions` must to be named", call. = FALSE)
    }
  }

  # no overlaps are allowed
  if (length(intersect(names(ellipsis), names(functions))))
  {
    stop("no overlaps between names in ... and `functions` are allowed",
         call. = FALSE)
  }

  # --- put all dependencies together and then extract each category one by one
  dependencies <- c(ellipsis, functions)

  # split functions and library dependencies
  i <- vapply(dependencies, is_library_dependency, logical(1))
  library_deps <- dependencies[i]
  dependencies <- dependencies[!d]

  # turn dependencies into names + pkg names
  if (length(dependencies)) {
    package_names <- vapply(library_deps, function(x) environmentName(environment(x)),
                       character(1))
    library_deps <- names(library_deps)
    names(library_deps) <- package_names
  }
  else {
    library_deps <- character()
  }

  # extract regular functions
  i <- vapply(dependencies, is.function, logical(1))
  function_deps <- dependencies[i]
  dependencies <- dependencies[!i]
  
  # remove environment from a function unless it's a closure
  function_deps <- lapply(function_deps, function (f) {
    if (identical(environment(f), eval_env)) {
      environment(f) <- emptyenv()
    }
    f
  })
  
  # there should be nothing left
  if (length(dependencies)) {
    stop('unprocessed dependencies left', call. = FALSE)
  }

  # TODO should return a self-contained function that can be run even
  #      without the `defer` package installed
  # TODO it can be called a class(f) == c("deferred", "function")
  # return the package object
  
  executor <- defer::executor
  exec_env <- environment(executor) <- new.env(parent = eval_env)
  
  exec_env$function_deps <- function_deps
  exec_env$library_deps  <- library_deps
  
  class(executor) <- c("deferred", "function")
  
  executor
}


make_all_named <- function (args)
{
  if (!is_all_named(args)) {
    stop("all objects passed via ... need to be named", call. = FALSE)
  }

  args
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




#' @description \code{is_execution_package} verifies if the given object
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
list_functions <- function (pkg)
{
  stopifnot(is_execution_package(pkg))
  return(names(pkg$functions))
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
list_dependencies <- function (pkg)
{
  stopifnot(is_execution_package(pkg))
  return(pkg$dependencies)
}

