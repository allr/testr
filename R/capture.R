#' Decorates function to capture calls and return values
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @param package name of package to look for function
#' @param verbose if to print additional output
#' @seealso write_capture
#'
decorate <- function(func, package, verbose)
{
    if (identical(class(library), "function") && getRversion() < '3.3.0') {
        suppressMessages(trace(library,
                               exit = quote(if (!missing(package)) testr:::refresh_decoration(package)),
                               print = FALSE))
    }
    if (!cache$trace_replaced && getRversion() < '3.3.0') {
        replace_trace()
    }
    if(class(func) != "character" || (!missing(package) && class(package) != "character")){
        stop("wrong argument type!")
    }
    if (is.na(package)){
        package <- utils::find(func)
        if (length(package) == 0) {
            warning(sprintf("Can't determine a package for function '%s'. If function is hidden, use package param",
                            func))
            return(invisible())
        } else {
            if (length(package) > 1) {
                warning("Function found in multiple packages, supply the exact name")
                return(invisible())
            }
        }
        if (package != ".GlobalEnv") {
            package <- substr(package, 9, nchar(package))
        } else {
            package <- NA
        }
    }
    if (is.na(package)) {
        isS3 <- is_s3_generic(func)
    } else {
        isS3 <- is_s3_generic(func, getNamespace(package))
    }
    if (isS3) {
        warning("Not decorating S3 generic")
        return(invisible())
    }
    write.call <- call("write_capture", if (is.na(package)) func else paste(package, func, sep = ":::"), quote(sys.frame(-4))) #nolint
    write.call[[1]] <- testr::write_capture
    tc <- call("trace",
               func,
               quote(write.call),
               print = testr_options("verbose"))
    hidden <- FALSE
    if (!func %in% ls(as.environment(if (is.na(package)) .GlobalEnv else paste("package", package, sep = ":")))) {
        if (!is.na(package))
            tc[["where"]] <- call("getNamespace", package)
        hidden <- TRUE
    }
    if (verbose) {
        eval(tc)
    } else {
        suppressMessages(eval(tc))
    }
    .decorated[[func]] <- list(func = func, package = package, hidden = hidden)
}

#' undecorate function
#'
#' @description Reset previously decorate function
#' @param func function name as a character string
#' @param verbose if to print additional output
#' @export
#' @seealso write_capture Decorate
#'
undecorate <- function(func, verbose)
{
    if (class(func) == "character"){
        fname <- func
    } else {
        stop("wrong argument type!")
    }
    ind <- which(fname %in% ls(.decorated, all.names = TRUE))
    if (length(ind) == 0) {
        stop(sprintf("Function %s was not decorated!", fname))
    }
    package <- .decorated[[func]]$package
    hidden <- .decorated[[func]]$hidden
    params <- list(fname)
    if (hidden)
        params[["where"]] <- call("getNamespace", package)
    if (verbose) {
        do.call(untrace, params)
    } else {
        suppressMessages(do.call(untrace, params))
    }
    rm(list = c(func), envir = .decorated)
}

#' Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#'
write_capture <- function(fname, args.env)
{
    if (!testr_options("capture.arguments"))
        return(NULL)
    .Call("testr_WriteCapInfo_cpp", PACKAGE = "testr", fname, args.env)
}

#' Setup information capturing for list of function
#'
#' @description This function is respinsible for setting up capturing for functions
#'
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @param package name of the package
#' @param verbose if to print additional status information
#' @seealso Decorate
#' @export
setup_capture <- function(flist, package, verbose = testr_options("verbose"))
{
    old <- testr_options("capture.arguments")
    if (old)
        testr_options("capture.arguments", FALSE)
    for (func in flist)
        if (eligible_capture(func))
            # TODO perhaps we want to put base in because these are builtins?
            decorate(func, NA_character_, verbose)
    if (old)
        testr_options("capture.arguments", TRUE)
}

#' Check if function is eligible for wrapping to capture arguments and return values
#'
#' @description This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in setup_capture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso setup_capture
eligible_capture <- function(func)
{
    return (!length(utils::getAnywhere(func)$objs) == 0
            && class(utils::getAnywhere(func)[1]) == "function"
            && !func %in% blacklist
            && !func %in% operators
            && !func %in% keywords
            && !func %in% sys
            && !func %in% env
            && !func %in% primitive_generics_fails)
}


#' Clear decoration
#'
#' @description Clear anything previously decorate
#' @param verbose if to print additional debugging information. Default \code{TRUE}.
#' @seealso undecorate
#' @export
clear_decoration <- function(verbose)
{
    for (fname in ls(.decorated, all.names = TRUE))
        undecorate(fname, verbose = verbose)
}
