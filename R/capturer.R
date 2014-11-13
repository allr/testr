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
                "textConnection", "scan", "require", "with"
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
#' @export
#' 
WriteCapInfo <- function(fname, args, retv, errs, warns){
  #   if (cache$writing.down)
  #     return(FALSE)
  #   else 
  #     cache$writing.down <- TRUE
  # tracing file setup
  #   fbody <- getAnywhere(fname)[1]
  #   fbody <- attr(fbody, "original")
  fbody <- NULL
  trace.file <- file.path(cache$trace.folder.path, paste(kCaptureFile, cache$capture.file.number, sep="."))
  if (!file.exists(trace.file))
    file.create(trace.file)
  else if (file.info(trace.file)$size > testrOptions('capture.file.size'))
    cache$capture.file.number <- cache$capture.file.number + 1 # improve here for while cycle
  
  builtin <- FALSE
  globals <- vector()
  if (!(fname %in% builtins())){
    #      globals <- codetools::findGlobals(fbody)
    #      globals <- globals[!globals %in% builtin.items]
    #      globals <- globals[!grepl("^C_", globals)] ## for external C functions
  } else {
    builtin <- TRUE
    fbody <- NULL
  }
  if (!is.environment(args) && length(args) > 0)
  for (i in 1:length(args))
    if (is.language(args[[i]])) args[[i]] <- as.expression(args[[i]])
  dargs <- deparse(args)
  #   da <- gsub("list", "alist", da)
  dargs <- gsub("\\*tmp\\*", "`*tmp*`", dargs)
  #   da <- gsub("^alist", "list", da)
  #   da <- gsub("^structure\\(alist", "structure(list", da)
  
  if (is.language(retv)){
    retv <- as.expression(retv)
  } else {
#    if (!is.environment(retv) && is.list(retv) && length(retv) > 0)
#    for (v in retv)
#      if (!missing(v) && is.language(v)) retv[which(retv == v)] <- as.expression(v)
  }
  dretv <- deparse(retv)
  #     dr <- gsub("list", "alist", dr)
  dretv <- gsub("\\*tmp\\*", "`*tmp*`", dretv)
  
  # printing
  print.capture <- function(x, prefix)
    if (x[1] != "NULL")
      if (length(x) < 100) sapply(x, function(y) cat(prefix, y, "\n", sep="")) else cat(prefix, "<too long>\n")

#  sink(trace.file, append = TRUE)
  
  for (g in globals){
    cat(kSymbPrefix, g, "\n", sep = "")
    cat(kValSPrefix, deparse(get(g)), "\n", sep = "")
  }

  cat(kFuncPrefix, fname, "\n", sep = "")
  print.capture(deparse(fbody), kBodyPrefix)
  print.capture(dargs, kArgsPrefix)
  print.capture(deparse(warns), kWarnPrefix)
  print.capture(dretv, kRetvPrefix)
  print.capture(deparse(errs), kErrsPrefix)
  cat("\n")
#  sink()
  #   cache$writing.down <- FALSE
}

DecorateBody <- function(func){
  # find function body
  function.body <- utils::getAnywhere(func)[1]
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

GenerateArgsFunction <- function(names.formals){
  args.names <- names.formals
  function.header <- paste("GetArgs <- function(", 
                           paste(args.names, collapse=","), 
                                 ")", 
                                 sep="")
  args.touch <- "args <- list();\n args.names <- vector();\n `_i` <- 1;\n args.list <- as.list(sys.call()[-1]);"
  if (length(args.names) > 0)
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
          } else {
            args.names <- c(args.names, names(dot.args))
          }
        }
      }\n"
        args.touch <- c(args.touch, code.template)
      } else {
        code.template <- "
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
        `_i` <- `_i` + 1;
        args.names <- c(args.names, '%s');\n";
        args.rep <- rep(args.names[i], 10)
        names(args.rep) <- NULL
        args.touch <- c(args.touch, do.call(sprintf, as.list(c(fmt=code.template, args.rep))))
      }
  args.touch <- c(function.header, "{", args.touch, "names(args) <- args.names;", "lapply(args, function(x) if(is.call(x)) enquote(x) else x)","}")
  args.code <- paste(args.touch, collapse = "\n")
  args.code
}
#' @title Decorate function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param func function name as a character string
#' @return decorated function
#' @seealso WriteCapInfo
#'
#' @export
ReplaceBody <- function(func){
  function.body <- utils::getAnywhere(func)[1]
  saved.function.body <- function.body
  fb <- NULL
  names.formals <- names(formals(function.body))
#   argument.pass <- "%s = if(!missing(%s)) 
#                             if(is.function(%s)) 
#                               substitute(%s)
#                             else %s
#                           else '_MissingArg'"
  argument.pass <- "%s = if(!missing(%s)) 
#                             if(is.function(%s)) 
#                               substitute(%s)
#   else 
                            %s
                            else '_MissingArg'"
  
  names.formals <- sapply(names.formals, function(x) if (grepl("^_", x)) paste("`", x, "`", sep='') else x)
  func.args <- parse(text=GenerateArgsFunction(names.formals))
  args.code <- parse(text=sprintf("environment(GetArgs) <- testr:::cache; args <- GetArgs(%s)", 
                                  paste(sapply(names.formals, 
                                               function(x) if(x != '...' && x != 'expr') 
                                                 sprintf(argument.pass, x, x, x, x, x) 
                                               else if (x == 'expr') 'substitute(expr)' 
                                               else x), 
                                        collapse = ",")))
                     
  if (!is.null(body(function.body))) {
    fb <- body(function.body)
#     early.return <- expression(if(testr:::cache$writing.down) return(return.value) else testr:::cache.writing.down <- TRUE)
#     cache.false <- expression(testr:::cache$writing.down <- FALSE)
    main.write.down <- parse(text=paste("environment(WriteCapInfo) <- testr:::cache; WriteCapInfo('",func,"',args, return.value, NULL, NULL)", sep=""))
    fb <- BodyReplace(fb, c(args.code, main.write.down))
    if (as.list(fb)[[1]] != '{'){
      last.line <- fb
      last.line <- parse(text=paste("return.value <- ", paste(deparse(last.line), collapse = ""), sep=""))
      fb <- as.call(c(as.name("{"), func.args, args.code, last.line, main.write.down, expression(return.value)))
    } else {
      last.line <- deparse(fb[[length(fb)]])
#       last.line <- parse(text=paste("return.value <- ", sprintf("tryCatch({%s})", paste(unlist(as.list(fb))[2:length(fb)], collapse =";")), sep=""))
      last.line <- parse(text=paste("return.value <- ", paste(last.line, collapse="\n"), sep=""))
      if ((length(fb) - 1) < 2){
        fb <- as.call(c(as.name("{"), func.args, args.code, last.line, main.write.down, expression(return.value)))
      }else{
        fb <- as.call(c(as.name("{"), func.args, args.code, unlist(as.list(fb))[2:(length(fb) - 1)], last.line, main.write.down, expression(return.value)))
      }
    }
    body(function.body) <- fb
  }
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
  gAobj <- utils::getAnywhere(fname) # getAnywhere object
  fobj <-  gAobj[1]
  prim.generics <- ls(.GenericArgsEnv)
  prim <- ls(.ArgsEnv)
#   if ((func %in% prim.generics))
#      return(NULL)
  where <- gAobj$where[1]
  if (!is.null(attr(fobj, "decorated")) && attr(fobj, "decorated"))
    warning(paste(fname, " was already decorated!"))
  else {
    env <- .GlobalEnv
    if (!identical(where, .GlobalEnv)){
      where <-  gsub("package:(.*)", "\\1", gAobj$where[1])
      env <- getNamespace(where)
      if (bindingIsLocked(fname, env = env))
        unlockBinding(fname, env = env)
    }
    if ("generic" %in% ftype(fobj) || fname %in% prim) { # check for generic, seems to be most consistent one
      if (capture.generics || fname %in% prim) 
        assign(fname, value = DecorateBody(fname), envir = env)
      else return(NULL)
    } else {
      assign(fname, value = ReplaceBody(fname), envir = env)
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
SetupCapture <- function(flist, verbose = testrOptions('verbose'), capture.generics = TRUE, capture.primitives = capture.primitives){
  if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
    dir.create(kCaptureFolder)
  set.cache("writing.down", TRUE)
#   cache$writing.down <- TRUE
  for (func in flist){
    if (EligibleForCapture(func)){
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
