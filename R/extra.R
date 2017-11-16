
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

  cat("Deferred-execution package\n\n")

  cat("Entry function:\n")
  cat(paste0("  ", format(ee$function_deps$entry), collapse = "\n"))

  formatted <- format_deferred(ee, summary = FALSE)
  if (nchar(formatted) > 0) {
    cat("\n\nIncludes ")
    cat(formatted)
  }
}


format_deferred <- function (x, ..., summary = FALSE)
{
  format_entry <- function (condition, title, text) {
    ifelse(!isTRUE(as.logical(condition)), "",
           paste0(title, ":\n",
                  paste(strwrap(text, prefix = "  "), collapse = "\n"),
                 "\n"))
  }

  function_names <- setdiff(names(x$function_deps), "entry")

  paste0(
    format_entry(length(x$arguments) && !isTRUE(summary),
                 "augmented arguments",
                 paste(names(x$arguments), collapse = ", ")),
    format_entry(length(function_names),
                 "functions",
                 paste(function_names, collapse = ", ")),
    format_entry(length(x$variables),
                 "variables",
                 paste(names(x$variables), collapse = ", ")),
    format_entry(nrow(x$library_deps),
                 "library calls",
                 paste0(x$library_deps$pkg, '::', x$library_deps$fun, collapse = ", "))
  )
}



#' @description \code{list_functions} returns a \code{character} vector
#' of names of functions packaged in \code{pkg}.
#'
#' @param df deferred function object.
#' @return A vector of function names.
#'
#' @export
#' @rdname extract
#'
extract_functions <- function (df)
{
  stopifnot(is_deferred(df))
  ee <- environment(df)
  return(ee$function_deps)
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



#' `list_dependencies` returns a `list` of variables referenced in any
#' of the packaged functions, that could be found in the caller
#' environment at the time of packaging.
#'
#' @return `list_dependencies` returns a named list.
#'
#' @export
#' @rdname extract
#'
extract_variables <- function (df)
{
  stopifnot(is_deferred(df))
  ee <- environment(df)
  return(ee$variables)
}
