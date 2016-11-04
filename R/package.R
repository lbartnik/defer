
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
  structure(list(), class = 'execution_package')
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
  return(character(0))
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
  return(character(0))
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

}
