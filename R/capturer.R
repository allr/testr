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
               ".handleSimpleError", 
               "tryCatch",
               "library", "UseMethod", 
               # something problematic
               "standardGeneric", "identity",
               "missing",
               "options", "ls", "sys.call", "stdout", "do.call", "cat", "withVisible",
               # messes up RStudio
               "textConnection", "require", "with", "get", "sink", "eval",
               "sprintf", "parse", "paste", 
               "textConnection", "require", "with", "get", "sink", "eval",
               "parse", "paste", "paste0", "evalq", "deparse", "exists", "environment", "conditionMessage.condition", "simpleError", "as.name",
               "attach", "attachNamespace", "lazyLoadDBexec", "lazyLoad", "lazyLoadDBfetch", "as.null.default", "asNamespace", "contributors", "close.connection",
               "close.srcfile", "close.srcfilealias", "computeRestarts", "findRestarts", "bindingIsLocked", "browserCondition", "browserSetDebug", "browserText", "closeAllConnections",
               "debugonce", "callCC", "delayedAssign", "detach", "browser", "clearPushBack", ".row_names_info", ".deparseOpts", ".makeMessage", ".libPaths", "%in%",
              "getNamespace", "isNamespace", "stdin", "stderr", "stop", "stopifnot", "structure", "merge.data.frame", "local",
              "match", "match.arg"
               
)

sys <- c('system.time','system.file','sys.status','sys.source','sys.save.image','sys.parents','sys.parent','sys.on.exit','sys.nframe','sys.load.image','sys.function','sys.frames','sys.frame','sys.calls','sys.call','R_system_version','.First.sys')
env <- c("environment", "environment<-", "parent.frame", "parent.env", "parent.env<-")
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(", ":", "%sep%", "[", "[[", "$", "@", "=", "[<-", "[[<-", "$<-", "@<-", "+", "-", "*", "/", 
               "^", "%%", "%*%", "%/%", "<", "<=", "==", "!=", ">=", ">", "|", "||", "&", "!")
builtin.items <- builtins()

code.template.dots <- "
        if (!missing(...)) {
          succ <- TRUE
          a <- tryCatch(list(...), error = function(x) succ <<- FALSE)
          if (succ){
            args <- c(args, a)
          } else {
            all.ind <- 1:length(args.list)
            if(length(ind) > 0) 
              ind <- all.ind[-ind] 
            else 
              ind <- all.ind
            for (i in ind){
              succ <- TRUE
              e <- tryCatch(eval(args.list[[i]]), error=function(x) succ <<- FALSE)
              j <- ifelse(is.null(names.args.list) || names.args.list[i] == '', length(args) + 1, names.args.list[i])
              if (succ && !is.function(e)){
                if (is.null(e)) {
                  args[j] <- list(NULL)
                } else {
                  args[[j]] <- e
                }
              } else {
                args[[j]] <- args.list[[i]]
              }
            }
          }
        }\n"

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
#  cat(fname, "\n")
  if (cache$writing.down)
    return(NULL);
  .Call('testr_WriteCapInfo_cpp', PACKAGE = 'testr', fname, args, retv, errs, warns)
}


#' @title Decorate function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param func function name as a character string
#' @return decorated function
#' @seealso WriteCapInfo
#'
#' @export
DecorateBody <- function(func, function.body){
  # create a wrapper closure and return in
  decorated.function <- function(...){}
  if (!is.null(formals(function.body))){    
    func <- ExtractFunctionName(func)
    names.formals <- names(formals(function.body))
    formals(decorated.function) <- formals(function.body) 
  } else {
    names.formals <- c("...")
  }
  argument.pass <- "%s = missing(%s)"
  names.formals.rcpp <- sapply(names.formals, ChangeNames)
  args.code <- expression(missingArgs <- list())
  if (length(names.formals) > 0){
    get.args.arguments <- vector()
    for (i in 1:length(names.formals)){
      get.args.arguments <- c(get.args.arguments, 
                              if (names.formals[i] != '...') 
                                sprintf(argument.pass, names.formals[i], names.formals[i], names.formals[i]))
    }
    args.code <- parse(text=sprintf("missingArgs <- list(%s)", paste(get.args.arguments, collapse = ",")))
  }  
  get.args.code <- parse(text="args <- tryCatch(testr:::GetArgs(sys.frame(), missingArgs, environment()), error=function(x) {});")
  initializations <- expression(warns <- NULL)
#   args.enquote <- expression(args <- lapply(args, function(x) if (is.language(x)) enquote(x) else x))
  envir.change <- expression(if (!is.null(body(function.body))) 
    body(function.body) <- as.call(c(
      as.name("{"), 
      expression(for(`_n` in ls(sys.frame(-3), all.names=TRUE)) if (grepl("^[.][a_zA-Z]",`_n`)) assign(`_n`, get(`_n`, sys.frame(-3)))), 
      body(function.body))))
  ret.value <- expression(
    return.value <- withCallingHandlers(                     
      do.call(function.body, args, envir = environment()),           
      error = function(e) {
        errs <- e$message
        WriteCapInfo(func, args, NULL, errs, warns)
        stop(errs)
      },
      warning = function(w) {
        if (is.null(warns))
          warns <<- w$message
        else 
          warns <<- c(warns, w$message)
      })
  )
  # special fix for cbind, somehow do.call loses colnames
  #   cbind.hack <- expression(if(func == "cbind") return.value <- function.body(..., deparse.level = deparse.level))
  main.write.down <- expression(WriteCapInfo(func, args, return.value, NULL, warns))
  function.return <- expression(return.value)  
  body(decorated.function) <- as.call(c(as.name("{"), 
                                        args.code, 
                                        get.args.code,
                                        initializations, 
                                        envir.change, 
#                                         args.enquote,
                                        ret.value,
#                                         cbind.hack,
                                        main.write.down,
                                        function.return))
  attr(decorated.function, "decorated") <- TRUE
  return (decorated.function)
}

BodyReplace <- function(where.replace, by.what){
  if (length(where.replace) == 1)
    return (where.replace)
  where.replace <- as.list(where.replace)
  if (where.replace[[1]] == 'function')
    return(as.call(where.replace))
  for (i in 1:length(where.replace)){
    if (length(as.list(where.replace[[i]])) > 2) {
      where.replace[[i]] <- as.call(BodyReplace(where.replace[[i]], by.what))
    } else {
      if (length(where.replace[[i]]) > 1)
        if (where.replace[[i]][[1]] == 'return'){
          last.line <- parse(text=paste("return.value <- ", paste(deparse(where.replace[[i]][[2]]), collapse = "\n"), ";\n", sep=""))
          where.replace[[i]] <- as.call(c(as.name("{"), last.line, by.what, expression(return(return.value))))
        }
    }
  }
  as.call(where.replace)
}

ChangeNames <- function(x){
  x <- if (grepl("^_|<-", x)) paste("`", x, "`", sep='') else x
  x <- if (x == '...') "dotArgs" else x
  x <- gsub("\\.", "", x)
  x
}

#' @title Replace function body with decoration hooks 
#' 
#' This function is responsible for changing function body to capture specific hooks
#' @param func function name as a character string
#' @return decorated function
#' @seealso WriteCapInfo
#'
#' @export
ReplaceBody <- function(func, function.body){
  if (is.null(body(function.body))) return(NULL);
  names.formals <- names(formals(function.body))
  argument.pass <- "%s = missing(%s)"
  names.formals <- sapply(names.formals, function(x) if (grepl("^_|<-", x)) paste("`", x, "`", sep='') else x)
  args.code <- expression(missingArgs <- list())
  if (length(names.formals) > 0){
    get.args.arguments <- vector()
    for (i in 1:length(names.formals)){
      get.args.arguments <- c(get.args.arguments, 
                              if (names.formals[i] != '...') 
                                sprintf(argument.pass, names.formals[i], names.formals[i], names.formals[i]))
    }
    args.code <- parse(text=sprintf("missingArgs <- list(%s)", paste(get.args.arguments, collapse = ",")))
  }  
  get.args.code <- parse(text="args <- GetArgs1(missingArgs, environment())")
  if (!is.null(body(function.body))) {
    main.write.down <- parse(text=paste("WriteCapInfo('",func,"',args, return.value, NULL, NULL)", sep=""))
    new.fb <- BodyReplace(body(function.body), c(args.code, main.write.down))
    last.line <- if (as.list(new.fb)[[1]] != '{') new.fb else new.fb[[length(new.fb)]]
    last.line <- parse(text=sprintf("return.value <- %s\n", paste(deparse(last.line), collapse = "\n")))
    code <- if (length(new.fb) > 2 && as.list(new.fb)[[1]] == '{') unlist(as.list(new.fb))[2:(length(new.fb) - 1)] else ""
    new.fb <- as.call(c(as.name("{"), 
                    args.code, 
                    get.args.code,
                    parse(text=code), 
                    last.line, 
                    main.write.down, 
                    expression(return.value)))
  }
  body(function.body) <- new.fb
  attr(function.body, "decorated") <- TRUE
  #   attr(function.body, "original.body") <- saved.function.body
  function.body
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
  invisible(.Call('testr_DecorateSubst_cpp', PACKAGE = 'testr', search(), fname, 
        if (is.null(cache$function.types[[fname]])) "function" else cache$function.types[[fname]]))
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
