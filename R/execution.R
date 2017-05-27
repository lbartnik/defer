

executor <- function ()
{
  # fist save the arguments
  args <- c(as.list(environment()), list(...))

  ee <- parent.env(environment())
  stopifnot(exists("function_deps", envir = ee, inherits = FALSE),
            exists("library_deps", envir = ee, inherits = FALSE),
            exists("variables", envir = ee, inherits = FALSE))
  stopifnot('entry' %in% names(function_deps))

  # create the execution environment
  exec_env <- new.env(parent = parent.frame(2))
  
  # make sure each function will search in exec_env by setting either
  # it parent (regular functions) or grandparent (closures) to exec_env
  process_fun <- function(f, n) {
    # do not remove environment from a closure; package_ sets env
    # to emptyenv() unless it's a closure
    if (identical(environment(f), emptyenv()))
      environment(f) <- exec_env
    else
      parent.env(environment(f)) <- exec_env
    
    assign(n, f, envir = exec_env)
  }
  mapply(process_fun, f = function_deps, n = names(function_deps))

  # set variables
  mapply(function(v, n) {
    assign(n, v, envir = exec_env)
  }, v = variables, n = names(variables))
  
    
  # TODO add library deps
  
  # make the call and pass arguments
  do.call('entry', args, envir = exec_env)
}


#' @export
run_deferred <- function (df, ...)
{
  stopifnot(is_deferred(df))
  df(...)
}


#' @export
print.deferred <- function (x)
{
  stopifnot(is.function(x))
  ee <- environment(x)

  cat("Deferred-execution function\n")
  
  cat("Dependencies: ", setdiff(names(ee$function_deps), 'entry'))
  cat("\n\n")
  
  cat("Entry function:\n")
  
  print(ee$function_deps$entry)
}
