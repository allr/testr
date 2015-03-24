## general (temporary) storage for testr's stuff
cache           <- new.env()
cache$capture.file.number <- 0
cache$writing.down <- FALSE

cache$arguments <- list()

#' @export
compiledArgsFunctions <- new.env()

#' @export
codeArgsFunctions <- new.env()

.onLoad <- function(libname, pkgname)
{
  if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
    dir.create(kCaptureFolder)
  cache$trace.folder.path <-  file.path(getwd(), kCaptureFolder)
  ## testr settings
  options('testr' = list(
    'verbose' = FALSE,
    'display.only.errors' = FALSE,
    'stop.on.error' = FALSE,
    'display.code.on.error' = FALSE,
    'file.summary' = FALSE,
    'capture.file.size' = 50 * 1000 * 1000
  ))
  require(codetools)
}

set.cache <- function(x, value){
  assign(x, value, envir = cache)
}

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
#' 
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
