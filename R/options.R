## general (temporary) storage for testr's stuff
cache           <- new.env()
cache$capture.file.number <- 0
cache$writing.down <- FALSE

cache$function.types <- list()

cache$prim.generics <- ls(.GenericArgsEnv)

cache$prim <- ls(.ArgsEnv) 

.onLoad <- function(libname, pkgname)
{
  if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
    dir.create(kCaptureFolder)
  cache$trace.folder.path <-  file.path(getwd(), kCaptureFolder)
  print("aaaaa\n"); 
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
  require(pryr)
  require(utils)
  
#   globals <- c('<<-','<-','-',':::','!','[','[<-','[[<-','{','+','as.list','c','enquote','if','.Internal','is.call','is.null','lapply','list','missing','names<-','substitute','sys.call','tryCatch','vector')  
#   for (elem in ls(getNamespace("testr"))) {
#     e <- tryCatch(get(elem, env = getNamespace("testr")), error = function(x) NULL)
#     if (is.function(e))
#       globals <- c(globals, findGlobals(e))
#   } 
# 
#   globals <- unique(globals)
#   globals <- intersect(builtins(), globals)
  globals <- builtins()
  for (elem in globals){
    e <- tryCatch(get(elem), error = function(x) NULL)
    if (is.function(e)){
      cache[[elem]] <- e
    }
  }
  cache$function.types <- readRDS("~/RProject/testr/function.types")
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
