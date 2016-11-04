
#' Create an execution package.
#'
#' If the entry-point function \code{entry} need to be called by a
#' different name in one of the other functions being packaged,
#' entry can be set to that name and the function itself can be
#' passed under its actual name.
#'
#' @param entry Entry-point function or a function name.
#' @param ... Other function to be packaged.
#' @param .funcs A list of functions.
#' @return An execution package object.
#'
#' @export
#' @rdname package
#' @seealso \code{\link{run_package}}
#'
package_ <- function (entry, ..., .funcs, .extract = FALSE)
{
  # TODO should library-function names be extracted even in the programmer's API?

  # prepare ellipsis
  ellipsis <- eval(substitute(alist(...)))
  ellipsis <- make_all_named(ellipsis)

  eval_env <- parent.frame()
  ellipsis <- lapply(ellipsis, function(x)eval(x, envir = eval_env))

  # prepare .funcs
  if (!missing(.funcs)) {
    if (!is_all_functions(.funcs)) {
      stop("only functions can be passed via .funcs", call. = FALSE)
    }
    if (!is_all_named(.funcs)) {
      stop("all elements in .funcs need to be named", call. = FALSE)
    }
  }
  else {
    .funcs <- list()
  }

  # rule out overlaps
  if (length(intersect(names(ellipsis), names(.funcs))) > 0) {
    stop("names in ... and .funcs cannot overlap", call. = FALSE)
  }

  functions <- c(ellipsis, .funcs)

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

  # entry
  if (is.character(entry)) {
    if ( !(entry %in% names(functions)) ) {
      stop("unknown function name passed via `entry`", call. = FALSE)
    }

    # entry is a simple function that passes all args to the actual
    # entry function specified here by name
    entry <- eval(substitute(function(...)X(...), list(X = as.name(entry))))
    environment(entry) <- emptyenv()
  }
  else {
    stopifnot(is.function(entry))
  }

  # remove environment from a function unless it's a closure
  functions <- c(list(entry = entry), functions)
  functions <- lapply(functions, function (f) {
    if (identical(environment(f), globalenv()))
      environment(f) <- emptyenv()
    f
  })

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
