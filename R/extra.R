
#' @export
print.deferred <- function (x, ...)
{
  stopifnot(is.function(x))
  ee <- environment(x)

  cat("Deferred-execution function\n")
  
  cat("Dependencies: ", setdiff(names(ee$function_deps), 'entry'))
  cat("\n\n")
  
  cat("Entry function:\n")
  
  print(ee$function_deps$entry)
}
