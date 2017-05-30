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
#' @rdname packaging
#'
#' @importFrom rlang quos eval_tidy caller_env
#'
defer_ <- function (entry, ..., .dots = list(), .extract = FALSE)
{
  # TODO should library-function names be extracted even in the programmer's API?

  # entry must be a regular function
  stopifnot(is.function(entry))
  stopifnot(is.list(.dots))
  
  # capture expressions with quos() and make sure all element are named
  dots  <- quos(...)
  dots  <- tryCatch(eval_tidy(make_all_named(dots)), error = function(e) stop(
    "some arguments passed in ... are not named and names cannot be auto-generated", call. = FALSE))
  .dots <- tryCatch(eval_tidy(make_all_named(.dots)), error = function(e) stop(
    "some elements in `.dots` are not named and names cannot be auto-generated", call. = FALSE))

  # no overlaps are allowed
  if (length(intersect(names(dots), names(.dots)))) {
    stop("names in ... and `.dots` cannot overlap", call. = FALSE)
  }
  if ('entry' %in% names(.dots)) {
    stop('cannot use `entry` as a name in `.dots`', call. = FALSE)
  }

  # --- put all dependencies together and then extract each category one by one
  deps <- c(dots, .dots, list(entry = entry))
  eval_env <- caller_env()
  
  processor <- DependencyProcessor$new(deps, eval_env)
  processor$run(.extract)

  # --- prepare and return the deferred execution function object
  
  executor <- defer:::executor
  exec_env <- environment(executor) <- new.env(parent = eval_env)
  
  exec_env$function_deps <- processor$function_deps
  exec_env$library_deps  <- processor$library_deps
  exec_env$variables     <- processor$variable_deps
  
  formals(executor) <- formals(deps$entry)
  if (match("...", names(formals(executor)), 0) == 0) {
    formals(executor) <- c(formals(executor), alist(...=))  
  }

  class(executor) <- c("deferred", "function")
  
  executor
}


#' @description \code{is_deferred} verifies if the given object
#' is an execution package.
#'
#' @param x Object to be tested.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @rdname packaging
#'
is_deferred <- function (x) inherits(x, 'deferred')


#' @description \code{list_functions} returns a \code{character} vector
#' of names of functions packaged in \code{pkg}.
#'
#' @param pkg An execution package object.
#' @return A vector of function names.
#'
#' @export
#' @rdname packaging
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
#' @rdname packaging
#'
extract_dependencies <- function (df)
{
  stopifnot(is_deferred(df))
  ee <- environment(df)
  return(ee$library_deps)
}


# ---------------------------------------------------------------------

#' @importFrom rlang UQE
make_all_named <- function (args)
{
  is_double_colon <- function (x) is.call(x) && identical(x[[1]], bquote(`::`))
  into_name       <- function (x) {
    e <- UQE(x)
    if (is.name(e)) return(as.character(e))
    if (is_double_colon(e)) return(deparse(e[[3]]))
    ""
  }

  if (is.null(names(args)) || !length(names(args))) {
    names(args) <- rep("", length(args))
  }
  
  empty <- !nchar(names(args))
  if (!any(empty)) return(args)
  
  new_names <- vapply(args[empty], into_name, character(1))

  if (any(!nchar(new_names))) {
    stop("some objects are not named and names cannot be auto-generated",
         call. = FALSE)
  }

  names(args)[empty] <- new_names
  args
}


is_library_dependency <- function (x)
{
  is.function(x) && isNamespace(environment(x))
}

is_closure <- function (x, caller_env)
{
  !identical(environment(x), caller_env) &&
    !identical(environment(x), globalenv())
}


library(R6)

#' @importFrom rlang caller_env
#' @importFrom R6 R6Class
DependencyProcessor<- R6::R6Class("DependencyProcessor",
  public = list(
    library_deps  = data.frame(pkg = character(), fun = character(), ver = character(),
                               stringsAsFactors = FALSE),
    function_deps = list(),
    variable_deps = list(),
    
    initialize = function (deps, caller_env) {
      private$deps <- deps
      private$caller_env <- caller_env
    },
    
    # 1. extract regular functions
    # 2. extract variables
    # 3. extract library functions
    # 4. nothing else should be left
    #
    run = function (extract = FALSE)
    {
      private$extract <- extract
      private$process()
    }
  ),
  private = list(
    deps       = list(),
    processed  = list(),
    caller_env = NA,
    extract    = FALSE,

    process = function () {
      while (length(private$deps)) {
        name    <- names(private$deps)[1]
        current <- private$deps[[1]]
        private$deps <- private$deps[-1]
        
        if (is_library_dependency(current)) {
          private$process_library(name, current)
        }
        else if (is.function(current)) {
          private$process_function(name, current)
        }
        else if (is.vector(current) || is.list(current)) {
          private$process_variable(name, current)
        }
        else {
          stop("cannot process")
        }
      }
    },
    
    process_library = function (name, fun) {
      pkg_name <- environmentName(environment(fun))
      pkg_ver  <- as.character(getNamespaceVersion(pkg_name))
      new_dep  <- data.frame(fun = name, pkg = pkg_name, ver = pkg_ver, stringsAsFactors = FALSE)

      self$library_deps <- rbind(self$library_deps, new_dep)
    },
    
    # Extracts regular functions.
    # remove environment from a function unless it's a closure
    #
    process_function = function (name, fun) {
      if (!is_closure(fun, private$caller_env)) {
        environment(fun) <- emptyenv()
      }
      self$function_deps[[name]] <- fun
      if (isTRUE(private$extract)) private$process_body(body(fun))
    },
    
    process_variable = function (name, value) {
      self$variable_deps[[name]] <- value
    },
    
    # https://stackoverflow.com/questions/14276728/finding-the-names-of-all-functions-in-an-r-expression/14295659#14295659
    process_body = function (x) {
      
      recurse <- function(x) sort(unique(as.character(unlist(lapply(x, private$process_body)))))
      
      already_found <- function (x) (f_name %in% c(names(self$function_deps), self$library_deps$fun, names(self$deps)))
      
      if (is.name(x)) {
        v_name <- as.character(x)
        if (nchar(v_name) && exists(v_name, envir = private$caller_env, mode = "numeric", inherits = TRUE)) {
          self$variable_deps[[v_name]] <- get(v_name, envir = private$caller_env)
        }
      }
      else if (is.call(x)) {
        f_name <- as.character(x[[1]])
        if (already_found(f_name)) {
          return(recurse(x[-1]))
        }
        
        if (exists(f_name, envir = private$caller_env, mode = 'function', inherits = TRUE)) {
          f_obj <- get(f_name, envir = private$caller_env)
          if (!is.primitive(f_obj)) {
            private$deps[[f_name]] <- f_obj
          }
        }
        
        return(recurse(x[-1]))
      }
      
      if (is.recursive(x)) recurse(x)
    }
  )
)






