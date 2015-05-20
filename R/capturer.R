#' @title Decorates function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @param package name of package to look for function
#' @export 
#' @seealso WriteCapInfo
#'
Decorate <- function(func, package) {
  if(class(func) != "character" || (!missing(package) && class(package) != "character")){
    stop("wrong argument type!")
  }
  if (missing(package)){
    package <- gsub("(.*):(.*)", "\\2", find(func))
    if (length(package) == 0)
      stop("Can't determine a package for function. If function is hidden, use package param")
    if (length(package) > 1)
      stop("Function found in multiple packages, supply the exact name")
  }
  if (IsS3Generic(func, getNamespace(package))) {
    warning("Not decorating S3 generic")
    return(invisible())
  }
  write.call <- call("WriteCapInfo", paste(package, func, sep=":::"), quote(sys.frame(-4)))
  tc <- call('trace', 
             func, 
             quote(write.call),
             print=quote(testrOptions('verbose')))
  hidden <- FALSE
  if (!func %in% ls(as.environment(paste("package", package, sep=":")))) {
    tc[["where"]] <- call('getNamespace', package)
    hidden <- TRUE
  }
  eval(tc)
  .decorated[[func]] <- list(func=func, package=package, hidden=hidden)
} 

#' @title Undecorate function
#' 
#' Reset previously decorate function
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Undecorate <- function(func) {
  if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }  
  ind <- which(fname %in% ls(.decorated))
  if (length(ind) == 0)
    stop("Function was not decorated!")
  package <- .decorated[[func]]$package
  hidden <- .decorated[[func]]$hidden
  params <- list(fname)
  if (hidden)
    list[["where"]] <- call('getNamespace', package)
  do.call(untrace, params)
  rm(list=c(func), envir=.decorated)
}

#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#' 
WriteCapInfo <- function(fname, args.env){
  if (!testrOptions('capture.arguments'))
    return(NULL)
  .Call('testr_WriteCapInfo_cpp', PACKAGE = 'testr', fname, args.env)
}

#' @title Setup information capturing for list of function
#' 
#' This function is respinsible for setting up capturing for functions
#' 
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @seealso Decorate
#' @export
SetupCapture <- function(flist, package){
  testrOptions('capture.arguments', FALSE)
  for (func in flist)
    if (EligibleForCapture(func))
      Decorate(func, package)
  testrOptions('capture.arguments', TRUE)
}

#' @title Check if function is eligible for wrapping to capture arguments and return values
#' 
#' This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in SetupCapture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso SetupCapture
EligibleForCapture <- function(func){
  return (!length(utils::getAnywhere(func)$objs) == 0 &&
            class(utils::getAnywhere(func)[1]) == "function" &&
            !func %in% blacklist &&
            !func %in% operators &&
            !func %in% keywords &&
            !func %in% sys &&
            !func %in% env && 
            !func %in% primitive.generics.fails)
}

#' @title Setup capture of builtin functions
#' 
#' Sets up capturing of builtin functions
#' @param internal wheather only internals should be captured, or all builtins
#' @param functions list of functions to be decorate
#' @param indexes specific indexes from functions vector
#' @param package
#' @seealso SetupCapture, Decorate
#' @export
BeginBuiltinCapture <- function(internal = FALSE, functions = builtins(internal), indexes, package){
  if (missing(indexes))
    SetupCapture(functions, package)
  else
    SetupCapture(functions[indexes], package)
}

#' @title Clear decoration
#' 
#' Clear anything previously decorate
#' @seealso Undecorate
#' @export
ClearDecoration <- function() {
  for (fname in ls(.decorated))
    Undecorate(fname)
}
