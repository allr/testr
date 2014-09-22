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
               # weird things happend when trying to unlock binding. Some of those might be wrong
               
#                "unlockBinding", "which", "sys.call", 
#                "strsplit", "structure", "stopifnot", "topenv", "stdout", "search",
                "match",
                "options", "ls", "sys.call", "stdout", "cat", "do.call", "match.call", 
                # messes up RStudio
                "textConnection", "scan", "require"
                # fails in glm(y1 ~ offset(y2))
#                 "lapply",
                # str(pl <- as.pairlist(ps.options()))
#                 "formals"
          )
#                )

need.replacement <- c("[<-.data.frame", "[<-", "[.data.frame", "transform.data.frame", "[.factor", "formals", "body", "assign", "with.default")
sys <- c('system.time','system.file','sys.status','sys.source','sys.save.image','sys.parents','sys.parent','sys.on.exit','sys.nframe','sys.load.image','sys.function','sys.frames','sys.frame','sys.calls','sys.call','R_system_version','.First.sys')
env <- c("environment", "environment<-", "parent.frame", "parent.env", "parent.env<-")
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
#   if (cache$writing.down)
#     return(FALSE)
#   else 
#     cache$writing.down <- TRUE
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
#   cache$writing.down <- FALSE
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
    # transform.default _data fix
    args.names <- sapply(args.names, function(x) if (grepl("^_", x)) paste("`", x, "`", sep='') else x)
    args.touch <- "args <- list(); args.names <- vector(); `_i` <- 1; args.list <- as.list(sys.call()[-1]);"
#     touch.arg <- function(arg, arg.name){
#       if (!missing(arg)) {
#         args.names <<- c(args.names, arg.name)
#         i <<- i + 1
#         if (is.null(arg)) {
#           list(NULL)
#         }
#         else {
#           arg
#         }
#       }
#     }
    
    for (i in 1:length(args.names))
      if (args.names[i] == '...'){
        args.touch <- c(args.touch, paste("if(!missing(`...`)){", 
                                            "dot.args <- list(...);", 
                                            "args <- c(args, dot.args);", 
                                            "`_i` <- `_i` + length(dot.args);",
                                            "if (is.null(names(dot.args))) {",
                                              "args.names <- c(args.names, rep('', length(dot.args)));",
                                            "} else {",
                                              "args.names <- c(args.names, names(dot.args))",
                                            "}",
                                          "}", sep=""))
                                                    
      } else {
                args.touch <- c(args.touch, 
                        paste("if(!missing(", args.names[i] ,")) {",
                              paste("if(", "is.null(", args.names[i], ")){", 
                                    paste("args[`_i`] <- list(NULL)\n", sep = ""), 
                                    "} else {",
                                    paste("args[[`_i`]]", " <- ", args.names[i], sep =""),
                                    "}\n", paste("; args.names <- c(args.names, '", args.names[i],"'); `_i` <- `_i` + 1;", sep = ""),sep=""), 
                              "} else {", 
                                paste("if (`_i` < length(args.list)){", 
                                      paste(
                                        paste("args[[`_i`]] <- args.list[[`_i`]];\n"),
                                        paste("args.names <- c(args.names, '');\n", sep = "") ,
                                        paste("`_i` <- `_i` + 1;\n"),
                                        sep = ""),
                                      "}", sep="")
                              ,"};",
                              sep = "")) 
      }
  
    args.touch <- c(args.touch, "names(args) <- args.names;", "args <- lapply(args, function(x) if(is.call(x)) enquote(x) else x)")
    args.code <- paste(args.touch, collapse = "")
    args.code.expression <- parse(text = args.code)
args.list <- vector()
for (i in 1:length(args.names))
  if (args.names[i] == '...')
    
    args.list <- c(args.list, "...")
else
  args.list <- c(args.list, paste(args.names[i], "= if(!missing(", args.names[i], ")) ", args.names[i], sep = ""))
# function.call <- parse(text=paste("function.body(", paste(args.list, collapse = ","), ")", sep=""))

  } else {
    args.code.expression <- expression(args <- list(...))
    function.call <- expression(if (cache$writing.down) return(function.body(...)))
  }
  initializations <- expression(warns <- NULL)

  envir.change1 <- expression(environment(function.body) <- environment())
  envir.change <- expression(if (!is.null(body(function.body))) body(function.body) <- 
                               as.call(c(as.name("{"), expression(for(`_n` in ls(sys.frame(-3), all.names=TRUE)) if (grepl("^[.][a_zA-Z]",`_n`)) assign(`_n`, get(`_n`, sys.frame(-3)))), 
                                         body(function.body)))
    )  
#       do.call(function.body, args, envir = environment(), quote = TRUE), ",
  quote.v <- expression(qt <- !(func %in% c("colSums", "matrix")))
  ret.value <- expression(

  return.value <- withCallingHandlers(                     
    do.call(function.body, args, quote = qt),           
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
    })
)
    # special fix for cbind, somehow do.call loses colnames
  special.hacks <- expression(  
    if (func == "cbind")
        return.value <- function.body(..., deparse.level = deparse.level)    
  )
#   print.name <- expression(cat("Capturing function - ", func, "Caller - ", as.list(sys.call(-1))[[1]], "\n"))
  print.name <- expression(cat("Capturing function - ", func,"\n"))
#   early.return <- parse(text=paste("if (cache$writing.down){ x <- ", "do.call(function.body, args, envir = environment(), quote = qt)", "; return(x);}", sep=""))
  main.write.down <- expression(WriteCapInfo(func, function.body, args, return.value, NULL, warns))
  cache.change <- expression(cache$writing.down <- TRUE)
  cache.unchange <- expression(cache$writing.down <- FALSE)
  function.return <- expression(return.value)  
  body(decorated.function) <- as.call(c(as.name("{"), 
#                                         print.name,
#                                         early.return,
                                        cache.change,
                                        args.code.expression, 
                                        initializations, 
#                                         envir.change1,
                                        envir.change, 
                                        quote.v,
                                        ret.value,
                                        special.hacks,
                                        main.write.down,
#                                         cache.unchange,
                                        function.return))
  attr(decorated.function, "decorated") <- TRUE
  return (decorated.function)
}

#' @export 
DecorateR <- function(func){
  function.body <- utils::getAnywhere(func)[1]
  fb <- NULL
  args.code <- expression(args <- list())
  if (!is.null(formals(function.body))){    
    args.names <- names(formals(function.body))
    # transform.default _data fix
    args.names <- sapply(args.names, function(x) if (grepl("^_", x)) paste("`", x, "`", sep='') else x)
    args.touch <- "args <- list(); args.names <- vector(); `_i` <- 1; args.list <- as.list(sys.call()[-1]);"
    for (i in 1:length(args.names))
      if (args.names[i] == '...'){
        code.template <- "
if (!missing(...)) {
  succ <- TRUE
  tryCatch(list(...), error=function(x) succ <<- FALSE)
        if (!succ){
        args[[`_i`]] <- substitute(...)
        } else {
        dot.args <- list(...)
        args <- c(args, dot.args)
        `_i` <- `_i` + length(dot.args)
        if (is.null(names(dot.args))) {
        args.names <- c(args.names, rep('', length(dot.args)))
        }
        else {
        args.names <- c(args.names, names(dot.args))
        }
        }}\n"
        args.touch <- c(args.touch, code.template)
        
      } else {
        code.template <- "
if (!missing(%s)) {
  succ <- TRUE
  tryCatch(%s, error=function(x) succ <<- FALSE)
  if (!succ){
    args[[`_i`]] <- substitute(%s)
  } else {
    if (is.null(%s)) {
      args[`_i`] <- list(NULL)
    } else {
      args[[`_i`]] <- %s
    }
  }
  args.names <- c(args.names, '%s');
  `_i` <- `_i` + 1;
} else {
    if (`_i` < length(args.list)) {
      args[[`_i`]] <- args.list[[`_i`]]
      args.names <- c(args.names, '')
      `_i` <- `_i` + 1
    } 
};\n";
        args.rep <- rep(args.names[i], 6)
        names(args.rep) <- NULL
        args.touch <- c(args.touch, do.call(sprintf, as.list(c(fmt=code.template, args.rep))))
    }
    args.touch <- c(args.touch, "names(args) <- args.names;", "args <- lapply(args, function(x) if(is.call(x)) enquote(x) else x)")
    args.code <-paste(args.touch, collapse = "")
  }
  args.code.expression <- parse(text = args.code)
  if (!is.null(body(function.body))) {
    fb <- body(function.body)
    main.write.down <- parse(text=paste("testr:::WriteCapInfo('",func,"',",paste("body('",func,"')", sep=""),",args, return.value, NULL, NULL)", sep=""))
    if (length(deparse(fb)) == 1 || fb[[1]] == '.Internal'){
      last.line <- fb
      last.line <- parse(text=paste("return.value <- ", deparse(last.line), sep=""))
      fb <- as.call(c(as.name("{"), last.line, args.code.expression, main.write.down, expression(return.value)))
    } else {
      last.line <- deparse(fb[[length(fb)]])
      last.line <- parse(text=paste("return.value <- ", paste(last.line, collapse="\n"), sep=""))
      fb[[length(fb)]] <- as.call(c(as.name("{"), last.line, args.code.expression, main.write.down, expression(return.value)))
    }
    body(function.body) <- fb
  }
  attr(function.body, "decorated") <- TRUE
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
    gAobj <- utils::getAnywhere(fname) # getAnywhere object
    fobj <-  gAobj[1]
    if ("generic" %in% ftype(fobj))
      return(NULL)
    where <- gAobj$where[1]
    if (!is.null(attr(fobj, "decorated")) && attr(fobj, "decorated"))
      warning("Functions was already decorated!")
    else {
      if (where != '.GlobalEnv'){
        where <-  gsub("package:(.*)", "\\1", gAobj$where[1])
        env <- getNamespace(where)
#         if (bindingIsLocked(fname, env = env))
#           unlockBinding(fname, env = env)
#         assign(fname, value = Decorate(fname), envir = .GlobalEnv)
#        assign(fname, value = Decorate(fname), envir = env)  
#       } else {
        if (fname %in% need.replacement){
          assign(fname, value = DecorateR(fname), envir = envir)
        }else{
          assign(fname, value = DecorateR(fname), envir = envir)
        }
      }
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
  set.cache("writing.down", TRUE)
#   cache$writing.down <- TRUE
  for (func in flist){
    if (EligibleForCapture(func)){
      if (!is.null(DecorateSubst(func)) && verbose)
        cat("capturing - ", func, "\n")
      
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
BeginBuiltinCapture <- function(internal = FALSE, indexes = 1:length(builtins(internal))){
  SetupCapture(builtins(internal)[indexes], verbose = TRUE)
}
