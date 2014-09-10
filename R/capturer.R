kCaptureFile <- "capture"
kCaptureFolder <- "capture"
kSymbPrefix <- "symb: "
kValSPrefix <- "vsym: "
kFuncPrefix <- "func: "
kBodyPrefix <- "body: "
kTypePrefix <- "type: "
kArgsPrefix <- "args: "
kRetvPrefix <- "retv: "
kErrsPrefix <- "errs: "
kWarnPrefix <- "warn: "
blacklist <- c("builtins", "rm", "source", "~", "<-", "$", "<<-", "&&", "||" ,"{", "(", 
               ".GlobalEnv", ".Internal", ".Primitive", "::", ":::", "substitute", "list", 
               ".Machine", 
               "debug", "undebug",
               "withCallingHandlers", "quote", ".signalSimpleWarning", "..getNamespace", ".External", ".External2", 
               "c", "try", 
               "assign", # because assignment is in parent.frame
               "NextMethod", # no idea why
               "setwd", # path of capture files are relative to WD, change that
               "save", "load", #no idea why
               "deparse",
               "rawConnection",
               "remove",
               "eval", "attach", 
               "match.arg",
               "Summary.Date",
               ".handleSimpleError", 
               "tryCatch",
               "detach",
               "library", "sink",
#                "sys.parent", "unlockBinding", "which", "sys.call", 
#                "strsplit", "structure", "stopifnot", "topenv", "stdout", "parent.frame", "match", "search", 
               "environment"
               )
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(", ":", "%sep%", "[", "[[", "$", "@", "=", "[<-", "[[<-", "$<-", "@<-", "+", "-", "*", "/", 
               "^", "%%", "%*%", "%/%", "<", "<=", "==", "!=", ">=", ">", "|", "||", "&", "!")
for.testing <- c("all", "any", "identical", "is.list")
builtin.items <- builtins()

#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param fbody function body
#' @param args arguments to function call
#' @param retv return value of a specified function call with arguments
#' @param errs caught errors during function call
#' @param warns caught warnings during function call
#' @seealso Decorate
#' 
WriteCapInfo <- function(fname, fbody, args, retv, errs, warns){
  if (cache$writing.down)
    return(FALSE)
  else 
    cache$writing.down <- TRUE
  # tracing file setup
  trace.file <- file.path(cache$trace.folder.path, paste(kCaptureFile, cache$capture.file.number, sep="."))
  if (!file.exists(trace.file))
    file.create(trace.file)
  else if (file.info(trace.file)$size > testrOptions('capture.file.size'))
    cache$capture.file.number <- cache$capture.file.number + 1 # improve here for while cycle

  builtin <- FALSE
  globals <- vector()
  if (!(fname %in% builtins())){
     globals <- codetools::findGlobals(fbody)
     globals <- globals[!globals %in% builtin.items]
     globals <- globals[!grepl("^C_", globals)] ## for external C functions
  } else {
    builtin <- TRUE
    fbody <- NULL
  }
  # printing
  sink(trace.file, append = TRUE)
  for (g in globals){
    cat(kSymbPrefix, g, "\n", sep = "")
    cat(kValSPrefix, deparse(get(g)), "\n", sep = "")
  }
  cat(kFuncPrefix, fname, "\n", sep = "")
  if (!builtin)
    fbody <- deparse(fbody)
  for (sline in fbody)
    cat(kBodyPrefix, sline, "\n", sep = "")
  cat(kArgsPrefix, deparse(args), "\n", sep = "")
  if (!is.null(warns))
    for (line in deparse(warns))
      cat(kWarnPrefix, line, "\n", sep = "")
  if (is.null(errs))
    cat(kRetvPrefix, deparse(retv), "\n", sep = "")
  else
    for (line in errs)
      cat(kErrsPrefix, line, "\n", sep = "")
  cat("\n")
  sink()
  cache$writing.down <- FALSE
}

#' @title Decorate function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param func function name as a character string
#' @return decorated function
#' @seealso WriteCapInfo
#'
Decorate <- function(func){
  # find function body
  function.body <- utils::getAnywhere(func)[1]
  # create a wrapper closure and return in
  decorated.function <- function(...){}
#   if (!typeof(function.body) %in% c('special', 'builtin')){
  if (!is.null(formals(function.body))){    
    formals(decorated.function) <- formals(function.body) 
    args.names <- names(formals(function.body))
    args.touch <- "args <- list(); args.names <- vector(); i <- 1;"
    for (i in 1:length(args.names))
      if (args.names[i] == '...'){
        args.touch <- c(args.touch, "dot.args <- list(...); args <- c(args, dot.args); i <- i + length(dot.args); args.names <- c(args.names, rep('', length(dot.args)));")        
      } else {
        args.touch <- c(args.touch, 
                        paste("if(!missing(",args.names[i] ,")) {", sep = ""), 
                        paste("\nif(is.null(", args.names[i], "))", paste("{args[i] <- list(NULL)\n", sep = ""), 
                              "}else{",
                              "args[[i]]", " <- ", args.names[i], sep = ""), "};", paste("args.names <- c(args.names, '", args.names[i],"'); i <- i + 1;}\n", sep = ""))
      }
  
    args.touch <- c(args.touch, "names(args) <- args.names")
    args.code <- paste(args.touch, collapse = "")
    args.code.expression <- parse(text = args.code)
  } else {
    args.code.expression <- expression(args <- list(...)) 
  }
  initializations <- expression(warns <- NULL)
  envir.change <- expression(environment(function.body) <- environment())
  ret.value <- expression(
    return.value <- withCallingHandlers(
      do.call(function.body, args, envir = environment()), 
      error = function(e) {
        errs <- e$message
        WriteCapInfo(func, function.body, args, NULL, errs, warns)
        stop(errs)
      },
      warning = function(w) {
        if (is.null(warns))
          warns <<- w$message
        else 
          warns <<- c(warns, w$message)
      }))
    
    # special fix for cbind, somehow do.call loses colnames
  special.hacks <- expression(  
    if (func == "cbind")
        return.value <- function.body(...)    
  )
  main.write.down <- expression(WriteCapInfo(func, function.body, args, return.value, NULL, warns))
  function.return <- expression(return.value)  
  body(decorated.function) <- as.call(c(as.name("{"), 
                                        args.code.expression, 
                                        initializations, 
                                        envir.change, 
                                        ret.value,
                                        special.hacks,
                                        main.write.down,
                                        function.return))
  attr(decorated.function, "decorated") <- TRUE
  return (decorated.function)
}

#' @title Decorates function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
DecorateSubst <- function(func, envir = .GlobalEnv){
    if (class(func) == "function"){
      fname <- as.character(substitute(func))
    } else if (class(func) == "character"){
      fname <- func
    } else {
      stop("wrong argument type!")
    } 
    gAobj <- utils::getAnywhere(fname) # getAnywhere object
    fobj <-  gAobj[1]
#     where <- gAobj$where[1]
    if (!is.null(attr(fobj, "decorated")) && attr(fobj, "decorated"))
      warning("Functions was already decorated!")
    else {
#       if (where != '.GlobalEnv'){
#         where <-  gsub("package:(.*)", "\\1", gAobj$where[1])
#         unlockBinding(fname, env = getNamespace(where))
# #         assign(fname, value = Decorate(fname), envir = .GlobalEnv)
#       assign(fname, value = Decorate(fname), envir = getNamespace(where))  
#       } else {
        assign(fname, value = Decorate(fname), envir = envir)
#       }
    } 
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
  cache$writing.down <- TRUE
  for (func in flist){
    if (EligibleForCapture(func)){
      if (verbose)
        cat("capturing - ", func, "\n")
      DecorateSubst(func)
    }
  }
  cache$writing.down <- FALSE
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
        !func %in% keywords)
}
  
#' @title Setup capture of builtin functions
#' 
#' Sets up capturing of builtin functions
#' @param internal wheather only internals should be captured, or all builtins
#' @seealso SetupCapture
#' @export
BeginBuiltinCapture <- function(internal = FALSE){
  SetupCapture(builtins(internal), verbose = TRUE)
}
