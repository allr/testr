kCaptureFile <- "capture"
kCaptureFolder <- "capture"
kSymbPrefix <- "symb: "
kValSPrefix <- "vsym: "
kFuncPrefix <- "func: "
kBodyPrefix <- "body: "
kTypePrefix <- "type: "
kArgsPrefix <- "argv: "
kRetvPrefix <- "retv: "
kErrsPrefix <- "errs: "
kWarnPrefix <- "warn: "
blacklist <- c("builtins", "rm", "source", "~", "<-", "$", "<<-", "&&", "||" ,"{", "(", 
               ".GlobalEnv", ".Internal", ".Primitive", "::", ":::", "substitute", "list", 
               ".Machine", "on.exit", 
               "debug", "undebug",
               "withCallingHandlers", "quote", ".signalSimpleWarning", "..getNamespace", ".External", ".External2", 
               "c", "try", 
               "NextMethod", # no idea why
               "setwd", # path of capture files are relative to WD, change that
               "rawConnection",
               ".handleSimpleError", "tryCatch",
               "library", "UseMethod", 
               # something problematic
               "standardGeneric", "identity","missing",
               "options", "ls", "sys.call", "stdout", "do.call", "cat", "withVisible",
               # messes up RStudio
               "sprintf", "parse", "paste", 
               "textConnection", "require", "with", "get", "sink", "eval",
               "parse", "paste", "paste0", "evalq", "deparse", "exists", "environment", "conditionMessage.condition", "simpleError", "as.name",
               "attach", "attachNamespace", "lazyLoadDBexec", "lazyLoad", "lazyLoadDBfetch", "as.null.default", "asNamespace", "contributors", "close.connection",
               "close.srcfile", "close.srcfilealias", "computeRestarts", "findRestarts", "bindingIsLocked", "browserCondition", "browserSetDebug", "browserText", "closeAllConnections",
               "debugonce", "callCC", "delayedAssign", "detach", "browser", "clearPushBack", ".row_names_info", ".deparseOpts", ".makeMessage", ".libPaths", "%in%",
               "getNamespace", "isNamespace", "stdin", "stderr", "stop", "stopifnot", "structure", "local", "merge.data.frame", 
               "match", "match.arg", "typeof", "conditionCall.condition", "withRestarts", "formals",
               # for .Primitive and functions without body
               ".C", ".Call", ".External", ".External.graphics", ".External2", ".Fortran",
               "as.call", "names<-", "names", "length", "is.pairlist", "is.null", "is.list", "invisible", "class<-", "class", 
               "baseenv", "attributes<-", "as.environment", "as.character", ".Call.graphics" , 
               "length<-", "call", "attr<-", "switch", "log2", "nargs", "as.numeric",
#                "xtfrm", "as.double","rep", "round", "max", "min",
               "attributes", "attributes<-", "is.language"
)

sys <- c('system.time','system.file','sys.status','sys.source','sys.save.image','sys.parents','sys.parent','sys.on.exit','sys.nframe','sys.load.image','sys.function','sys.frames','sys.frame','sys.calls','sys.call','R_system_version','.First.sys')
env <- c("environment", "environment<-", "parent.frame", "parent.env", "parent.env<-")
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(", ":", "%sep%", "[", "[[", "$", "@", "=", "[<-", "[[<-", "$<-", "@<-", "+", "-", "*", "/", 
               "^", "%%", "%*%", "%/%", "<", "<=", "==", "!=", ">=", ">", "|", "||", "&", "!")

#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args arguments to function call
#' @param retv return value of a specified function call with arguments
#' @param errs caught errors during function call
#' @param warns caught warnings during function call
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#' 
WriteCapInfo <- function(fname, args, retv, errs, warns){
  if (cache$writing.down)
    return(NULL);
  .Call('testr_WriteCapInfo_cpp', PACKAGE = 'testr', fname, args, retv, errs, warns)
}

#' @title Decorates function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Decorate <- function(func, envir = .GlobalEnv){
  if (class(func) == "function"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }    
  exit.capturer <- function() {
    testr:::WriteCapInfo(fname, 
                   args = cache$arguments[[length(cache$arguments)]], 
                   retv = NULL, errs = NULL, warns = NULL) 
    cache$arguments <- cache$arguments[-length(cache$arguments)]
  }
  entry.capturer <- function() {
    cache$arguments[[length(arguments) + 1]] <- testr:::GetArgs(sys.frame(sys.nframe() - 5))
  } 
  do.call(trace, list(what=fname, tracer=entry.capturer, exit=exit.capturer, print = FALSE))
} 

#' @title Decorates function to capture call argumens 
#' 
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Undecorate <- function(func) {
  if (class(func) == "function"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }  
  do.call(untrace, fname)
}


#' @title Setup information capturing for list of function
#' 
#' This function is respinsible for setting up capturing for functions
#' 
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @param verbose if to print what functions will be captured
#' @seealso Decorate
#' @export
SetupCapture <- function(flist, verbose = testrOptions('verbose')){
  if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
    dir.create(kCaptureFolder)
  set.cache("writing.down", TRUE)
  for (func in flist){
    if (EligibleForCapture(func)){
      DecorateSubst(func)
    }
  }
  set.cache("writing.down", FALSE)
}

#' @title Check if function is eligible for wrapping to capture arguments and return values
#' 
#' This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in SetupCapture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso SetupCapture
EligibleForCapture <- function(func){
  return (!length(getAnywhere(func)$objs) == 0 &&
            class(getAnywhere(func)[1]) == "function" &&
            !func %in% blacklist &&
            !func %in% operators &&
            !func %in% keywords &&
            !func %in% sys &&
            !func %in% env)
}

#' @title Setup capture of builtin functions
#' 
#' Sets up capturing of builtin functions
#' @param internal wheather only internals should be captured, or all builtins
#' @seealso SetupCapture
#' @export
BeginBuiltinCapture <- function(internal = FALSE, functions = builtins(internal)){
  SetupCapture(functions, verbose = TRUE)
}
