
#' Printer for deferred functions.
#' 
#' @description \code{print.defer} provides a specialized \code{print}
#' method for deferred function wrappers.
#' 
#' @param x object to be printed out.
#' @param ... further arguments passed to or from other methods.
#' 
#' @export
#' 
print.deferred <- function (x, ...)
{
  stopifnot(is.function(x), is_deferred(x))
  ee <- environment(x)

  cat("Deferred-execution function\n")
  
  cat("Dependencies: ", setdiff(names(ee$function_deps), 'entry'))
  cat("\n\n")
  
  cat("Entry function:\n")
  
  print(ee$function_deps$entry)
}



#' @description \code{list_functions} returns a \code{character} vector
#' of names of functions packaged in \code{pkg}.
#'
#' @param pkg An execution package object.
#' @return A vector of function names.
#'
#' @export
#' @rdname extract
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
#' @rdname extract
#'
extract_dependencies <- function (df)
{
  stopifnot(is_deferred(df))
  ee <- environment(df)
  return(ee$library_deps)
}

