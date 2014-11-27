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
               "save", "load", #no idea why
               "deparse",
               "rawConnection",
               "remove",
               "eval", "attach", 
               "match.arg",
               "pmatch",
               ".handleSimpleError", 
               "tryCatch",
               "detach",
               "library", "sink", "UseMethod", "unloadNamespace",
               # something problematic
               "loadNamespace", "loadedNamespaces", "loadNamespaceInfo", "load", "unloadNamespace", "standardGeneric", "identity","missing",
               # weird things happend when trying to unlock binding. Some of those might be wrong
               "match",
               "options", "ls", "sys.call", "stdout", "cat", "do.call", "match.call", 
               # messes up RStudio
               "textConnection", "scan", "require", "with", "get", "writeLines"
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
    formals(decorated.function) <- formals(function.body) 
    args.names <- names(formals(function.body))
    args.names <- sapply(args.names, function(x) if (grepl("^_", x)) paste("`", x, "`", sep='') else x)
    args.touch <- "args <- list();
    args.names <- names(formals(function.body));
    ind <- vector();
    args.list <- as.list(match.call()[-1]);
    names.args.list <- names(args.list)"
    for (i in 1:length(args.names)){
      if (args.names[i] != '...'){
        code.template <- "
        if (!missing(%s)) {
        succ <- TRUE
        e <- tryCatch(%s, error=function(x) succ <<- FALSE)
        if (succ){
        if (is.null(e)) {
        if (grepl('`', '%s'))
        args[%s] <- list(NULL)
        else 
        args['%s'] <- list(NULL)
        } else {
        args$%s <- e
        }
        }
        ind <- c(ind, which(names.args.list=='%s'))
        }\n";
        args.rep <- rep(args.names[i], 7)
        names(args.rep) <- NULL
        args.touch <- c(args.touch, do.call(sprintf, as.list(c(fmt=code.template, args.rep))))
      }
    }
    if ('...' %in% args.names){
      args.touch <- c(args.touch, code.template.dots)
    } 
    args.code <- paste(args.touch,  collapse = "")
    #     args.code <- paste(args.code, "\nnames(args) <- names(args.list);\n")
    args.code.expression <- parse(text = args.code)
  } else {
    args.code.expression <- parse(text=paste("args <- list();
                                             args.names <- names(formals(function.body));
                                             ind <- vector();
                                             args.list <- as.list(match.call()[-1]);
                                             names.args.list <- names(args.list)", code.template.dots))
  }
  initializations <- expression(warns <- NULL)
  #  envir.change <- expression(environment(function.body) <- environment())
  args.enquote <- expression(args <- lapply(args, function(x) if (is.language(x)) enquote(x) else x))
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
                                        args.code.expression, 
                                        initializations, 
                                        envir.change, 
                                        args.enquote,
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
  x <- if (grepl("^_", x)) paste("`", x, "`", sep='') else x
  x <- if (x == '...') "dotArgs" else x
  x <- gsub("\\.", "", x)
  x
}

GenerateArgsFunction <- function(names.formals){
  names.formals.rcpp <- sapply(names.formals, ChangeNames)

  function.header <- if (is.null(names.formals)) "SEXP GetArgs(Environment evalFrame)" 
                      else sprintf("SEXP GetArgs(Environment evalFrame, %s)", 
                                    paste(sapply(names.formals.rcpp, 
                                                 function(x) if (x != "dotArgs") sprintf("SEXP %s", x) else "List dotArgs"), 
                                    collapse = ","))
  initializations <- "List args;\n"
  args.touch <- vector()
  if (length(names.formals) > 0)
    for (i in 1:length(names.formals))
      if (names.formals[i] == '...'){
        code.template <- "
          int n = dotArgs.length();
          StringVector dotNames = dotArgs.attr(\"names\");
          StringVector names = args.attr(\"names\");
          for( int i=0; i<n; i++){
              args.push_back(Rcpp_eval(dotArgs[i], evalFrame)) ;    
              names.push_back(dotNames[i]);
          }
          args.attr(\"names\") = names; \n"
        args.touch <- c(args.touch, code.template)
      } else {
        code.template <- "
          try {
            %s = Rcpp_eval(%s, evalFrame);
          } catch(...) {
          }
          args[\"%s\"] = %s;"
        args.touch <- c(args.touch, sprintf(code.template, names.formals.rcpp[i], names.formals.rcpp[i], names.formals[i], names.formals.rcpp[i]))
      }
  function.body <- sprintf("{\n%s\n}", paste(initializations, paste(args.touch, collapse = "\n"), "return args;", sep="\n"))
  args.touch <- paste(function.header, function.body, sep = "")
  Rcpp::cppFunction(args.touch)
}

#' @title Decorate function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param func function name as a character string
#' @return decorated function
#' @seealso WriteCapInfo
#'
#' @export
ReplaceBody <- function(func, function.body){
  names.formals <- names(formals(function.body))
  argument.pass <- "%s = if(!missing(%s)) 
                            substitute(%s)
                            else '_MissingArg'"
  names.formals.rcpp <- sapply(names.formals, ChangeNames)
  
  compiledArgsFunctions[[func]]<- GenerateArgsFunction(names.formals)
  
  func.args <- parse(text=paste(
    sprintf("arg.names <- %s;", paste(deparse(names.formals), collapse ="")), 
    "dots <- as.list(match.call()[-1])
    dots <- dots[!names(dots) %in% arg.names];", sep = "\n")
  )
  get.args.arguments <- vector()
  args.code <- parse(text="args <- GetArgs(sys.frame())")
  if (length(names.formals) > 0){
    for (i in 1:length(names.formals)){
      get.args.arguments <- c(get.args.arguments, 
                            if (names.formals[i] != '...') 
                              sprintf("%s = %s", names.formals.rcpp[i], names.formals[i])
                            else
                              'dots')
    }
    args.code <- parse(text=sprintf("args <- GetArgs(sys.frame(), %s)", paste(get.args.arguments, collapse = ",")))
  }
  if (!is.null(body(function.body))) {
    main.write.down <- parse(text=paste("WriteCapInfo('",func,"',args, return.value, NULL, NULL)", sep=""))
    new.fb <- BodyReplace(body(function.body), c(args.code, main.write.down))
    last.line <- if (as.list(new.fb)[[1]] != '{') new.fb else new.fb[[length(new.fb)]]
    last.line <- parse(text=paste("return.value <- ", paste(deparse(last.line), collapse = "\n"), sep=""))
    code <- if (length(new.fb) > 2 && as.list(new.fb)[[1]] == '{') unlist(as.list(new.fb))[2:(length(new.fb) - 1)] else ""
    new.fb <- as.call(c(as.name("{"), 
                    parse(text=sprintf("GetArgs <- compiledArgsFunctions$%s;\n", func)), 
                    func.args, 
                    args.code, 
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
DecorateSubst <- function(func, envir = .GlobalEnv, capture.generics = TRUE, capture.primitives = TRUE){
  if (class(func) == "function"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }    
  #   if (!is.null(attr(function.obj, "decorated")) && attr(function.obj, "decorated"))
  #     warning(paste(fname, " was already decorated!"))
  .Call('testr_DecorateSubst_cpp', PACKAGE = 'testr', search(), fname, capture.generics, 
        cache$function.types[[fname]], cache$prim.generics, cache$prim)
} 



#' @title Setup information capturing for list of function
#' 
#' This function is respinsible for setting up capturing for functions
#' 
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @param verbose if to print what functions will be captured
#' @seealso Decorate
#' @export
SetupCapture <- function(flist, verbose = testrOptions('verbose'), capture.generics = TRUE, capture.primitives = capture.primitives){
  if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
    dir.create(kCaptureFolder)
  set.cache("writing.down", TRUE)
#     cache$writing.down <- TRUE
  for (func in flist){
    if (EligibleForCapture(func)){
      cat("capturing - ", func, "\n")
      if (!is.null(DecorateSubst(func, capture.generics = capture.generics, capture.primitives = capture.primitives)) && verbose)
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
BeginBuiltinCapture <- function(internal = FALSE, indexes = 1:length(builtins(internal)), capture.generics = TRUE, capture.primitives = TRUE){
  SetupCapture(builtins(internal)[indexes], verbose = TRUE, capture.generics = capture.generics, capture.primitives = capture.primitives)
}
