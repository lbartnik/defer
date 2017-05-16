
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
  
  # do not use list(...) because functions might be pointed by
  # names, e.g. defer(f, summary); in that case we first extract
  # the name (with make_all_named), and later the package name
  ellipsis <- eval(substitute(alist(...)))
  ellipsis <- make_all_named(ellipsis)

  # now extract the actual object
  eval_env <- parent.frame()
  ellipsis <- lapply(ellipsis, function(x)eval(x, envir = eval_env))

  # prepare `functions`; only actual functions are allowed
  if (length(functions)) {
    if (!is_all_functions(functions)) {
      stop("only function objects can be passed via `functions`", call. = FALSE)
    }
    if (!is_all_named(functions)) {
      stop("all elements in `functions` must to be named", call. = FALSE)
    }
  }

  # prepare `variables`; all objects are allowed
  if (length(variables)) {
    if (!is_all_named(variables)) {
      stop("all elements in `variables` must to be named", call. = FALSE)
    }
  }
  
  # no overlaps are allowed
  if (!is_empty(intersect(names(ellipsis), names(functions))) ||
      !is_empty(intersect(names(ellipsis), names(variables))) ||
      !is_empty(intersect(names(functions), names(variables))))
  {
    stop("no overlaps between names in ..., `functions` and `variables` are allowed",
         call. = FALSE)
  }

  # put all dependencies together and then extract each category one by one
  dependencies <- c(ellipsis, functions, variables)

  # split functions and library dependencies
  id <- vapply(functions, is_library_dependency, logical(1))
  dependencies <- functions[id]
  functions <- functions[!id]

  # turn dependencies into names + pkg names
  if (length(dependencies) > 0) {
    dep_pkgs <- vapply(dependencies, function(x) environmentName(environment(x)),
                       character(1))
    dependencies <- names(dependencies)
    names(dependencies) <- dep_pkgs
  }
  else {
    dependencies <- character()
  }

  # remove environment from a function unless it's a closure
  functions <- c(list(entry = entry), functions)
  functions <- lapply(functions, function (f) {
    if (identical(environment(f), globalenv()))
      environment(f) <- emptyenv()
    f
  })

  # TODO should return a self-contained function that can be run even
  #      without the `defer` package installed
  # TODO it can be called a class(f) == c("deferred", "function")
  # return the package object
  structure(
    list(functions = functions, dependencies = dependencies),
    class = 'execution_package'
  ) # structure
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
is_execution_package <- function (x) inherits(x, 'execution_package')


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



#' Run a function from the execution package.
#'
#' @param pkg Execution package object.
#' @param ... Parameters to the function being called.
#' @param .fun Name of the function being called.
#' @return The value returned in the call.
#'
#' @export
#' @seealso \code{\link{package_}}
#'
run_package <- function (pkg, ..., .fun = 'entry')
{
  stopifnot(is_execution_package(pkg))
  stopifnot(.fun %in% names(pkg$functions))

  # create the execution environment
  exec_env <- new.env(parent = parent.frame(2))

  # make sure each function will search in exec_env by setting either
  # it parent (regular functions) or grandparent (closures) to exec_env
  mapply(function(f, n) {
    # do not remove environment from a closure; package_ sets env
    # to emptyenv() unless it's a closure
    if (identical(environment(f), emptyenv()))
      environment(f) <- exec_env
    else
      parent.env(environment(f)) <- exec_env

    assign(n, f, envir = exec_env)
    T
  }, f = pkg$functions, n = names(pkg$functions))

  # make the call and pass arguments
  do.call(.fun, list(...), envir = exec_env)
}
