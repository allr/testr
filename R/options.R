## general (temporary) storage for testr's stuff
cache <- new.env()
cache$capture_num <- 0

cache$arguments <- list()
.decorated <- new.env()

kCaptureFile <- "capture"
kCaptureFolder <- "capture"
kSymbPrefix <- "symb: "
kValSPrefix <- "vsym: "
kFuncPrefix <- "func: "
kArgsPrefix <- "argv: "

blacklist <- c("builtins", "rm", "source", "~", "<-", "$", "<<-", "&&", "||" ,"{", "(",
               ".GlobalEnv", ".Internal", ".Primitive", "::", ":::", "substitute", "list",
               ".Machine", "on.exit", "debug", "undebug",
               "withCallingHandlers", "quote", ".signalSimpleWarning", "..getNamespace", ".External", ".External2",
               "c", "try", "NextMethod", "UseMethod",# no idea why
               "setwd", # path of capture files are relative to WD, change that
               "rawConnection", ".handleSimpleError", "tryCatch",
               "library", # something problematic
               "standardGeneric", "identity","missing",
               "options", "ls", "sys.call", "stdout", "do.call", "cat", "withVisible",
               "sprintf", "parse", "paste",
               "textConnection", "require", "with", "get", "sink", "eval",
               "evalq", "deparse", "exists", "environment", "conditionMessage.condition", "simpleError", "as.name",
               "attach", "attachNamespace", "lazyLoadDBexec", "lazyLoad", "lazyLoadDBfetch", "as.null.default",
               "asNamespace", "contributors", "close.connection",
               "close.srcfile", "close.srcfilealias", "computeRestarts", "findRestarts", "bindingIsLocked",
               "browserCondition", "browserSetDebug", "browserText", "closeAllConnections",
               "debugonce", "callCC", "delayedAssign", "detach", "browser", "clearPushBack", ".row_names_info",
               ".deparseOpts", ".makeMessage", ".libPaths", "%in%",
               "getNamespace", "isNamespace", "stdin", "stderr", "stop",
               "stopifnot", "structure", "local", "merge.data.frame",
               "match", "match.arg", "typeof", "conditionCall.condition", "withRestarts", "formals",
               # for .Primitive and functions without body
               ".C", ".Call", ".External", ".External.graphics", ".External2", ".Fortran",
               "as.call", "names<-", "names", "length",
               "is.pairlist", "is.null", "is.list", "invisible", "class<-", "class",
               "baseenv", "attributes<-", "as.environment", "as.character", ".Call.graphics",
               "length<-", "call", "attr<-", "switch", "log2", "nargs", "as.numeric",
               "attributes", "attributes<-", "is.language",
               # errors with trace
               "match.call", ".doTrace", "tracingState", "traceback", "trace"
)

sys <- c("system.time", "system.file", "sys.status",
         "sys.source", "sys.save.image", "sys.parents",
         "sys.parent", "sys.on.exit", "sys.nframe",
         "sys.load.image", "sys.function", "sys.frames",
         "sys.frame", "sys.calls", "sys.call", "R_system_version", ".First.sys")
env <- c("environment", "environment<-", "parent.frame", "parent.env", "parent.env<-")
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(", ":", "%sep%", "[", "[[", "$", "@", "=", "[<-",
               "[[<-", "$<-", "@<-", "+", "-", "*", "/",
               "^", "%%", "%*%", "%/%", "<", "<=", "==",
               "!=", ">=", ">", "|", "||", "&", "!")

primitive_generics_fails <- c(.S3PrimitiveGenerics, "round", "min", "max", "expression", "attr")

.onLoad <- function(libname, pkgname) {
    if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
        dir.create(kCaptureFolder)
    # make sure temp_dir is empty
    cache$temp_dir <- tempdir()
    clean_temp()
    cache$trace_path <-  file.path(getwd(), kCaptureFolder)
    ## testr settings
    options("testr" = list(
        "verbose" = FALSE,
        "display.only.errors" = FALSE,
        "stop.on.error" = FALSE,
        "display.code.on.error" = FALSE,
        "file.summary" = FALSE,
        "capture.file.size" = 50 * 1000 * 1000,
        "capture.arguments" = TRUE
    ))
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
testr_options <- function(o, value) {
    res <- getOption("testr")
    if (missing(value)) {
        ## just querying
        if (missing(o))
            return(res)
        if (o %in% names(res))
            return(res[[o]])
        stop("Wrong option queried.")
    } else {
        if (!o %in% names(res))
            stop(paste("Invalid option name:", o))
        res[[o]] <- value
        options("testr" = res)
    }
}
