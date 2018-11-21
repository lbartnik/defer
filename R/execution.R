

executor <- function (...)
{
  # fist save the arguments
  args <- base::c(as.list(environment()), list(...))

  ee <- parent.env(environment())
  stopifnot(exists("function_deps", envir = ee, inherits = FALSE),
            exists("library_deps", envir = ee, inherits = FALSE),
            exists("variables", envir = ee, inherits = FALSE),
            exists("arguments", envir = ee, inherits = FALSE))
  stopifnot('entry' %in% names(ee$function_deps))

  # create the execution environment
  libs_env <- new.env(parent = parent.frame(2))
  exec_env <- new.env(parent = libs_env)

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
  mapply(process_fun, f = ee$function_deps, n = names(ee$function_deps))

  # set variables
  mapply(function(v, n) {
    assign(n, v, envir = exec_env)
  }, v = ee$variables, n = names(ee$variables))


  # add library deps
  # load them into a separate environment in case the order of loading
  # packages matters
  process_library_dep <- function (fun, pkg, ver) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
      stop("cannot load package ", pkg, ", aborting...", call. = FALSE)
    }
    if (!identical(as.character(getNamespaceVersion(pkg)), ver)) {
      warning("version mismatch for package ", pkg, call. = FALSE)
    }
    fun_obj <- get(fun, envir = getNamespace(pkg), mode = "function", inherits = FALSE)
    if (is.null(fun_obj)) {
      stop("could not extract ", fun, " from package ", pkg, ", aborting...", call. = FALSE)
    }
    assign(fun, fun_obj, envir = libs_env)
  }
  with(ee$library_deps, {
    mapply(process_library_dep, fun = fun, pkg = pkg, ver = ver)
  })

  # set augmented argument values
  is_likely_missing <- function (value) is.symbol(value) && identical(as.character(value), "")
  i <- vapply(args, is_likely_missing, logical(1))

  j <- (names(args) %in% names(ee$arguments)) & !i
  if (any(j)) {
    stop("following arguments are already augmented: ",
         paste(names(args)[j], collapse = ", "), call. = FALSE)
  }
  for (name in names(ee$arguments)) {
    args[[name]] <- ee$arguments[[name]]
  }

  # nothing can be missing
  i <- vapply(args, is_likely_missing, logical(1))
  if (any(i)) {
    stop("missing arguments: ", paste(names(args)[i], collapse = ", "), call. = FALSE)
  }

  # make the call and pass arguments
  do.call('entry', args, envir = exec_env)
}


#' Run the deferred function.
#'
#' An explicit way to signify that a function being run is actually
#' a \code{deferred} function wrapper.
#'
#' @param df deferred function object.
#' @param ... parameters passed to \code{df}.
#' @return The return value of the wrapped function.
#'
#' @export
#'
run_deferred <- function (df, ...)
{
  stopifnot(is_deferred(df))
  df(...)
}
