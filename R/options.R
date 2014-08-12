.onLoad <- function(libname, pkgname)
{
  ## testr settings
  options('testr' = list(
    'verbose' = FALSE,
    'display.only.errors' = FALSE,
    'stop.on.error' = FALSE,
    'display.code.on.error' = FALSE,
    'file.summary' = FALSE,
    'capture.file.size' = 50 * 1000 * 1000
  ))
}

## general (temporary) storage for pander's stuff
cache           <- new.env()
cache$capture.file.number <- 0
cache$writing.down <- FALSE

#' Querying/setting testr option
#'
#' To list all \code{testr} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{testr} options are available:
#'
#' \itemize{
#'      \item \code{digits}: numeric (default: \code{2}) passed to \code{format}
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @seealso \code{\link{evalsOptions}}
#' @aliases pander.option
#' @note \code{pander.option} is deprecated and is to be removed in future releases.
#' @examples \dontrun{
#' panderOptions()
#' panderOptions('digits')
#' panderOptions('digits', 5)
#' }
testrOptions <- function(o, value) {
  
  res <- getOption('testr')
  
  ## just querying
  if (missing(value)) {
    
    if (missing(o))
      return(res)
    
    if (o %in% names(res))
      return(res[[o]])
    
    stop('Wrong option queried.')
    
  } else {
    
    if (!o %in% names(res))
      stop(paste('Invalid option name:', o))
    
    res[[o]] <- value
    options('testr' = res)
    
  }
  
}

#' @export
testr.option <- function(x, ...) {
  mc <- match.call(testrOptions)
  mc[[1]] <- quote(testrOptions)
  eval(mc)
}
