# the actual API

#' @title Enables capturing of the specified functions.
#'
#' The functions can be expressed as character literals in the form of either only a function name, or package name ::: function name, or directly as a symbol (i.e. only function name, or package name ::: function name).
#'
#' @param ... List of functions to capture, either character literals, or symbols
#' @param verbose TRUE to display additional information
#' @export
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
#' @export
capture_builtins <- function(internal = FALSE, verbose = testr_options("verbose")) {
    functions <- builtins(internal)
    setup_capture(functions, verbose = verbose)
}

#' @title Stops capturing the selected functions.
#'
#' @param ... Functions whose capture is to be dropped (uses the same format as capture)
#' @param verbose TRUE to display additional information
#' @export
stop_capture <- function(..., verbose = testr_options("verbose")) {
    for (f in parseFunctionNames(...))
        undecorate(f$name, f$package, verbose = verbose)
    invisible(NULL)
}

#' @title Stops capturing all currently captured functions.
#'
#' @param verbose TRUE to display additional information
#' @export
stop_capture_all <- function(verbose = testr_options("verbose")) {
    clear_decoration(verbose = verbose)
}

#' @title Generates tests from captured information.
#'
#' @param output_dir Directory to which the tests should be generated.
#' @param root Directory with the capture information, defaults to capture.
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param verbose TRUE to display additional information.
#' @export
generate <- function(output_dir, root = testr_options("capture.folder"), timed = F, verbose = testr_options("verbose")) {
    cache$output.dir <- output_dir
    test_gen(root, output_dir, timed, verbose = verbose);
}

#' @title Filters the generated tests so that only tests adding code coverage will be kept.
#'
#' Uses code coverage to remove tests... At least two, maybe three types of code coverage:
#'
#' - rcov for getting coverage of R code (here we need to annotate )
#'
#'
filter <- function(output_dir) {
    stop("NOT IMPLEMENTED")

}

#' @title Runs the generated tests.
#'
#' This function is a shorthand for calling testthat on the previously generated tests.
#'
#' @param test_dir Directory in which the tests are located. If empty, the last output directory for generate or filter functions is assumed.
#' @param verbose TRUE to display additional information.
#' @return TRUE if all tests passed, FALSE otherwise.
#' @export
run <- function(test_dir, verbose = testr_options("verbose")) {
    if (missing(test_dir)) {
        test_dir <- cache$output.dir
        if(is.na(test_dir))
            stop("Test directory must be specified, no testcases it cache yet")
    }
    result = TRUE
    # now we have the directory in which the tests are located, run testthat on them
    library(testthat, quietly = TRUE)
    # for all folders in the file
    dirs <- list.files(test_dir, include.dirs = T, no.. = T)
    for (d in dirs) {
        d <- file.path(test_dir, d, fsep = .Platform$file.sep)
        if (file.info(d)$isdir) {
            tryCatch(testthat::test_dir(d), error=function(x) {
                result <<- FALSE
                x #invisible(x)
            })
        }
    }
    result
}

# helpers -------------------------------------------------------------------------------------------------------------

# TODO these are also in evaluation.R, perhaps evaluation.R should die and its non-api should move to helpers, or someplace else?

#' @title Generates tests from given code and specific captured functions
#'
#'
#' @param code Code from which the tests will be generated.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... functions to be captured during the code execution (same syntax as capture function)
#' @export
testr_code <- function(code, output_dir, ...) {
    code <- substitute(code)
    capture(...)
    eval(code)
    stop_capture_all()
    generate(output_dir)
    invisible()
}

#' @title Generates tests by running given source file.
#'
#' @param src.root Source file to be executed and captured, or directory containing multiple files.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... Functions to be tested.
#' @export
testr_source <- function(src.root, output_dir, ...) {
    if (!file.exists(src.root))
        stop("Supplied source does not exist")
    if (file.info(src.root)$isdir)
        src.root <- list.files(src.root, pattern = "\\[rR]", recursive = T, full.names = T)
    capture(...)
    for (src.file in src.root)
        source(src.file, local = T)
    stop_capture_all()
    generate(output_dir)
    invisible()

}


# ideally this sholuld use existing downloaded package
testr_package <- function() {

}

# this should download the package from bioconductor
testr_bioconductor <- function() {


}

# this should download the package from cran
testr_cran <- function() {

}

# this should download the package from github
test_github <- function() {

}

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
    names(result) <- sapply(result, `[`, "name")
    result
}
