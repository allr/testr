# rm(list=ls())

# trace.file <- "closure.0"

# file.create(trace.file)
writing.down <<- FALSE

write.down <- function(f.name, args, body, ret){
  if (writing.down)
    return(FALSE)
  else 
    writing.down <<- TRUE
  symb.prefix <- "symb: "
  vsym.prefix <- "vsym: "
  func.prefix <- "func: "
  body.prefix <- "body: "
  type.prefix <- "type: "
  args.prefix <- "args: "
  retn.prefix <- "retn: "
  builtin <- FALSE
#   func <- deparse(sc[[1]])
  func <- f.name
  globals <- vector()
#   if (!(func %in% builtins())){
#     globals <- codetools::findGlobals(body)
#     globals.environments <- sapply(globals, pryr::where) 
#     globals.indexes <- sapply(globals.environments, identical, .GlobalEnv)
#     globals <- globals[globals.indexes]
#   } else {
    builtin <- TRUE
    body.prefix <- type.prefix
    if (func %in% builtins(TRUE))
      body <- "I"
    else body <- "P"
#   }
  # printing
  for (g in globals){
    cat(symb.prefix, g, "\n", file = trace.file, append = TRUE)
    cat(vsym.prefix, deparse(eval(parse(text=paste(".GlobalEnv$", g, sep = "")))), "\n", file = trace.file, append = TRUE)
  }
  cat(func.prefix, func, "\n", file = trace.file, append = TRUE)
  if (!builtin)
    body <- deparse(body)
  for (sline in body)
    cat(body.prefix, sline, "\n",file = trace.file, append = TRUE)
  cat(args.prefix, deparse(args), "\n", file = trace.file, append = TRUE)
  cat(retn.prefix, deparse(ret), "\n", file = trace.file, append = TRUE)
  cat("\n", file = trace.file, append = TRUE)
  writing.down <<- FALSE
}

decorate <- function(func.undec){
  if (class(func.undec) == "function"){
    force(func.undec)
    f.body <- func.undec
  }
  else if (class(func.undec) == "character" && length(func.undec) <= 1){
    assign(func.undec, utils::getAnywhere(func.undec)[1])
    f.body <- utils::getAnywhere(func.undec)[1]
  } else if (class(func.undec) == "character") {
    sapply(func.undec, decorate)
  } else {
    stop("wrong argument type!")
  } 
  f.name <- func.undec
#   cat("f.name - ", f.name, "\n")
  func.dec <- function(...){
    args <- list(...)
    res <- f.body(...) 
    sc <- match.call(expand.dots=TRUE)  
#     cat(sc[[1]],"\n")
    write.down(f.name, args, f.body, res)
    return(res)
  }
  # problem with lists for now
  return (func.dec)
  # eval(substitute(func.undec <- func.dec), envir=.GlobalEnv)
}

blacklist <- c("substr<-", "parent.env<-", "comment<-", 
               "parent.frame", "parent.env", "match.call", 
               "rm", "builtins", 
               "retracemem", "restartFormals", "restartDescription", "requireNamespace", "source", "~")
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(",":","%sep%","[","[[", "$","@", "<-", "<<-","=", "[<-","[[<-","$<-", "@<-", "+","-","*","/", "^","%%","%*%","%/%","<","<=","==","!=",">=",">","|","||","&","&&","!")

tested <- c("all", "any", "identical", "is.list")
# for (func in tested){
# for (func in builtins(TRUE)){
#   writing.down <<- TRUE
#   if (!length(getAnywhere(func)$objs) == 0 &&
#         !func %in% blacklist &&
#         !grepl("<-", func) &&
#         !grepl("^\\$", func) &&
#         !(func %in% operators) &&
#         !func %in% keywords){
#     cat(func, "\n")
#     assign(func, decorate(func), envir = .GlobalEnv)
#   }
#   writing.down <<- FALSE
# }
# id <- function(i) i
# decorate(any)
# source("~/Dropbox/RProject//R-3.0.1/tests/any-all.R")