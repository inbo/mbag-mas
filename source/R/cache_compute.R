#' Cache the result of an R expression to disk
#'
#' Evaluates an expression and stores its result on disk if no cached version
#' exists. On subsequent calls, the cached object is loaded and returned instead
#' of re-evaluating the expression. This is useful for expensive computations
#' such as bootstrapping, model fitting, or downloading data.
#'
#' The cache is stored as an `.rds` file by default using
#' [base::saveRDS()] and read using [base::readRDS()].
#'
#' @param path A character string giving the directory in which the cache file
#'   should be stored. The directory is created if it does not already exist.
#' @param file A character string giving the base name of the cache file,
#'   without extension.
#' @param expr An R expression to evaluate if the cache file does not exist.
#'   This argument is quoted and evaluated in the calling environment.
#' @param extension A character string giving the file extension to use.
#'   Defaults to `"rds"`. Currently, only `"rds"` is supported.
#'
#' @return
#' The object produced by evaluating `expr`, either freshly computed or loaded
#' from the cache.
#'
#' @details
#' If the cache file
#'
#' `file.path(path, paste0(file, ".", extension))`
#'
#' exists, the object is read from disk and returned immediately.
#' Otherwise, `expr` is evaluated, the resulting object is written to the cache
#' using [base::saveRDS()], and then returned.
#'
#' This function is intended for reproducible analysis workflows where
#' computationally intensive intermediate results can be reused across runs.
#'
#' @examples
#' cache_dir <- tempdir()
#'
#' result <- cache_compute(
#'   path = cache_dir,
#'   file = "example",
#'   expr = {
#'     Sys.sleep(2)  # expensive computation
#'     rnorm(10)
#'   }
#' )
#'
#' # The second call loads the cached result instead of recomputing it
#' result2 <- cache_compute(
#'   path = cache_dir,
#'   file = "example",
#'   expr = {
#'     stop("This expression will not be evaluated.")
#'   }
#' )

cache_compute <- function(
  path,
  file,
  expr,
  extension = "rds"
) {
  expr <- substitute(expr)

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  filename <- file.path(path, paste0(file, ".", extension))

  if (file.exists(filename)) {
    return(readRDS(filename))
  }

  value <- eval(expr, envir = parent.frame())

  saveRDS(value, filename)

  value
}
