# the actual API

#' @title Enables capturing of the specified functions.
#'
#' The functions can be expressed as character literals in the form of either only a function name, or package name ::: function name, or directly as a symbol (i.e. only function name, or package name ::: function name).
#'
#' @param ... List of functions to capture, either character literals, or symbols
#' @param verbose TRUE to display additional information
capture <- function(..., verbose = testr_options("verbose")) {
    old <- testr_options("capture.arguments")
    if (old)
        testr_options("capture.arguments", FALSE)
    for (f in parseFunctionNames(...))
        decorate(f[["name"]], f[["package"]], verbose = verbose)
    if (old)
        testr_options("capture.arguments", TRUE)
    invisible(NULL)
}

#' @title Enables capturing of all builtin functions.
#'
#' @param internal TRUE if only internal functions should be captured
#' @param verbose TRUE to display additional information
capture_builtins <- function(internal = FALSE, verbose = testr_options("verbose")) {
    functions <- builtins(internal)
    setup_capture(functions, verbose = verbose)
}

#' @title Stops capturing the selected functions.
#'
#' @param ... Functions whose capture is to be dropped (uses the same format as capture)
#' @param verbose TRUE to display additional information
stop_capture <- function(..., verbose = testr_options("verbose")) {
    for (f in parseFunctionNames(...))
        undecorate(f$name, f$package, verbose = verbose)
    invisible(NULL)
}

#' @title Stops capturing all currently captured functions.
#'
#' @param verbose TRUE to display additional information
stop_capture_all <- function(verbose = testr_options("verbose")) {
    clear_decoration(verbose = verbose)
}

#' @title Generates tests from captured information.
#'
#' @param output_dir Directory to which the tests should be generated.
#' @param root Directory with the capture information, defaults to capture.
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param vebose TRUE to display additional information.
generate <- function(output_dir, root = testr_options("capture.folder"), timed = F, verbose = testr_options("verbose")) {
    cache$output.dir <- output_dir
    test_gen(root, output_dir, timed, verbose = verbose);
}

#' @title Filters the generated tests so that only tests adding code coverage will be kept.
filter <- function(output_dir) {

}

#' @title Runs the generated tests.
#'
#' This function is a shorthand for calling testthat on the previously generated tests.
#'
#' @param test_dir Directory in which the tests are located. If empty, the last output directory for generate or filter functions is assumed.
#' @param verbose TRUE to display additional information.
#' @return TRUE if all tests passed, FALSE otherwise.
run <- function(test_dir, verbose = testr_options("verbose")) {
    if (missing(test_dir)) {
        test_dir <- cache$output.dir
        if(is.na(test_dir))
            stop("Test directory must be specified, no testcases it cache yet")
    }
    result = TRUE
    # now we have the directory in which the tests are located, run testthat on them
    library(testthat, quietly = TRUE)
    tryCatch(testthat::test_dir(test_dir), error=function(x) {
        result <<- FALSE
        invisible(x)
    })
    result
}

# helpers -------------------------------------------------------------------------------------------------------------





# anything below should not be exported -------------------------------------------------------------------------------

#' @title Parses given function names to a list of name, package characters. If package is not specified, NA is returned instead of its name.
#'
#' @param ... Functions either as character vectors, or package:::function expressions.
#' @return List of parsed package and function names as characters.
parseFunctionNames <- function(...) {
    args <- as.list(substitute(list(...)))[-1]
    i <- 1
    result <- list()
    result[length(args)] <- NULL
    while (i <= length(args)) {
        tryCatch({
            x <- eval(as.name(paste("..",i,sep="")))
            if (is.character(x)) {
                # it is a character vector, use its value
                x <- strsplit(x, ":::")[[1]]
                if (length(x) == 1)
                    x <- list(NA, x)
                if (x[[2]] == "")
                    x[[2]] <- ":::"
                result[[i]] <- c(name = x[[2]], package = x[[1]])
            } else {
                stop("Use substitured value")
            }
        }, error = function(e) {
            a <- args[[i]]
            if (is.name(a)) {
                result[[i]] <<- c(name = as.character(a), package = NA)
            } else if (is.language(a) && length(a) == 3 && a[[1]] == as.name(":::")) {
                result[[i]] <<- c(name = as.character(a[[3]]), package = as.character(a[[2]]))
            } else {
                print("error")
                stop(paste("Invalid argument index", i));
            }
        })
        i <- i + 1
    }
    result
}
