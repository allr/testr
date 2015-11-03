## general (temporary) storage for testr's stuff
cache <- new.env()
cache$capture_num <- 0
cache$trace_replaced <- FALSE

cache$arguments <- list()
.decorated <- new.env()

kCaptureFile <- "capture"
kCaptureFolder <- "capture"
kSymbPrefix <- "symb: "
kValSPrefix <- "vsym: "
kFuncPrefix <- "func: "
kArgsPrefix <- "argv: "

blacklist <- c(".GlobalEnv", ".Internal", ".Primitive", "substitute",
               ".Machine", "on.exit",
               "withCallingHandlers", "quote",
               "c", "NextMethod", "UseMethod", "standardGeneric", "identity","missing",
               "sys.call", "withVisible", "findRestarts", "local", "withRestarts", "formals",
               ".C", ".Call", ".External", ".External.graphics", ".External2", ".Fortran", ".Call.graphics",
               "length", "as.environment",
               "length<-", "call", "switch", "nargs", "as.numeric", "library.dynam.unload",
               "suppressMessages",
               # errors with trace
               "match.call", ".doTrace", "tracingState", "traceback", "trace", "get0",
               "forceAndCall", # added in R.3.2.1
               "library"
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
               "!=", ">=", ">", "|", "||", "&", "!", "~", 
               "<-", "$", "<<-", "&&", "||" ,"{", "(")

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
        "display_only_errors" = FALSE,
        "stop_on_error" = FALSE,
        "display_code_on_error" = FALSE,
        "file_summary" = FALSE,
        "capture_file_size" = 50 * 1000 * 1000,
        "capture.arguments" = TRUE,
        "IO"=TRUE,
        "parallel_tests"=TRUE,
        "rtests"=FALSE,
        "rprofile"='
.First <- function() {
        library(testr)
        library(utils)
        builtin_capture()
}'
    ))
    ## replace functions connected to trace with temporary versions
    # assign(".TraceWithMethods", as.environment("package:methods"))
    # unlockBinding(".TraceWithMethods", getNamespace("methods"))
    # environment(TraceWithMethods) <- getNamespace("methods")
    # assign(".TraceWithMethods", TraceWithMethods, getNamespace("methods"))
    # lockBinding(".TraceWithMethods", getNamespace("methods"))
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
