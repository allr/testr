#' @title Decorates function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @param package name of package to look for function
#' @param verbose if to print additional output
#' @export 
#' @seealso write_capture
#'
decorate <- function(func, package, verbose = testr_options("verbose")) {
    if(class(func) != "character" || (!missing(package) && class(package) != "character")){
        stop("wrong argument type!")
    }
    if (missing(package)){
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
        package <- substr(package, 9, nchar(package))
    }
    if (is_s3_generic(func, getNamespace(package))) {
        warning("Not decorating S3 generic")
        return(invisible())
    }
    write.call <- call("write_capture", paste(package, func, sep=":::"), quote(sys.frame(-4))) #nolint
    tc <- call("trace",
               func,
               quote(write.call),
               print = testr_options("verbose"))
    hidden <- FALSE
    if (!func %in% ls(as.environment(paste("package", package, sep=":")))) {
        tc[["where"]] <- call("getNamespace", package)
        hidden <- TRUE
    }
    if (verbose) {
        eval(tc)
    } else {
        suppressMessages(eval(tc))
    }
    .decorated[[func]] <- list(func=func, package=package, hidden=hidden)
}

#' @title undecorate function
#' 
#' Reset previously decorate function
#' @param func function name as a character string
#' @param verbose if to print additional output
#' @export 
#' @seealso write_capture Decorate
#'
undecorate <- function(func, verbose = testr_options("verbose")) {
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
    rm(list=c(func), envir=.decorated)
}

#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#' 
write_capture <- function(fname, args.env){
    if (!testr_options("capture.arguments"))
        return(NULL)
    .Call("testr_WriteCapInfo_cpp", PACKAGE = "testr", fname, args.env)
}

#' @title Setup information capturing for list of function
#' 
#' This function is respinsible for setting up capturing for functions
#' 
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @seealso Decorate
#' @export
setup_capture <- function(flist, package){
    testr_options("capture.arguments", FALSE)
    for (func in flist)
        if (eligible_capture(func))
            decorate(func, package)
    testr_options("capture.arguments", TRUE)
}

#' @title Check if function is eligible for wrapping to capture arguments and return values
#' 
#' This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in setup_capture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso setup_capture
eligible_capture <- function(func){
    return (!length(utils::getAnywhere(func)$objs) == 0
            && class(utils::getAnywhere(func)[1]) == "function"
            && !func %in% blacklist
            && !func %in% operators
            && !func %in% keywords
            && !func %in% sys
            && !func %in% env
            && !func %in% primitive_generics_fails)
}

#' @title Setup capture of builtin functions
#' 
#' Sets up capturing of builtin functions
#' @param internal wheather only internals should be captured, or all builtins
#' @param functions list of functions to be decorate
#' @param indexes specific indexes from functions vector
#' @param package
#' @seealso setup_capture, Decorate
#' @export
builtin_capture <- function(internal = FALSE, functions = builtins(internal), indexes, package){
    if (missing(indexes))
        setup_capture(functions, package)
    else
        setup_capture(functions[indexes], package)
}

#' @title Clear decoration
#' 
#' Clear anything previously decorate
#' @seealso undecorate
#' @export
clear_decoration <- function() {
    for (fname in ls(.decorated, all.names = TRUE))
        undecorate(fname)
}
