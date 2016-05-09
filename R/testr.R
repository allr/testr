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

#' @title Filter the generated tests so that only tests increasing code coverage will be kept.
#'
#' @description This function attempts to filter test cases based on code coverage collected by covr package.
#' Filtering is done in iterational way by measuring code coverage of every test separately and skipping the ones
#' that don't increase the coverage.
#'
#' @param test_root root directory of tests to be filtered
#' @param output_dir resulting directory where tests will be store. If nothing is supplied, tests that don't
#' increase coverage will be removed from test_root
#' @param ... functions that tests should be filtered aganist
#' @param package_path package root of the package that coverage should be measured
#' @param remove_tests if the tests that don't increase coverage should be removed. Default: \code{FALSE}.
#' This option will be set to \code{TRUE} if \code{output_dir} is not supplied
#' @param verbose whether the additional information should be displayed. Default: \code{TRUE}
#' @return NULL
#'
#' @export
filter <- function(test_root, output_dir, ...,
                   package_path = "", remove_tests = FALSE,
                   verbose = testr_options("verbose")) {
    functions <- parseFunctionNames(...)
    if (length(functions) && package_path != "") {
        stop("Both list of functions and package to be filtered aganist is supplied, please use one of the arguments")
    }
    if (missing(output_dir) && !remove_tests) {
        warning("output_directory was not supplied, so the tests that don't increase coverage will be removed from test_root")
        remove_tests <- TRUE
    }
    # convert functions into a list function=>package to use vectorize functions sapply/lapply
    fn <- sapply(functions, `[`, 1)
    functions <- sapply(functions, `[`, 2)
    names(functions) <- fn
    filter_tests(test_root, output_dir, functions, package_path, remove_test = remove_tests, verbose = verbose)
    invisible(NULL)
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


#' @title Generates tests for a package by running the code associated with it.
#'
#' Runs the examples, vignettes and possibly tests associated with the package and captures the usage of package's functions. Creates tests from the captured information, filters it according to the already existing tests and if any new tests are found, adds them to package's tests.
#'
#' @param package.dir Name/path to the package, uses devtools notation.
#' @param include.tests If TRUE, captures also execution of package's tests.
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param filter TRUE if generated tests should be filteres so that only those adding to a coverage will be used
#' @param verbose Prints additional information.
#' @export
#'
testr_package <- function(package.dir = ".", include.tests = FALSE, timed = FALSE, filter = TRUE, build = TRUE, output, verbose = testr_options("verbose")) {
    # stop all ongoing captures
    stop_capture_all()
    library(devtools, quietly = T)
    library(tools, quietly = T)
    if (build) {
        if (verbose)
            cat(paste("Building package", package.dir, "\n"))
        package.path = devtools::build(package.dir, quiet = T)
        if (verbose)
            cat(paste("  built into", package.dir, "\n"))
    } else {
        package.path = package.dir
    }
    # install the package
    if (verbose)
        cat(paste("Installing package", package.path, "\n"))
    #devtools:::install(package.name, quiet = T)
    install.packages(package.path, repos = NULL, quiet = T, type = "source")
    # get list of all functions
    package = devtools:::as.package(package.dir)
    library(package = package$package, character.only = T)
    if (verbose)
        cat(paste("Package", package$package, "installed\n"))
    # get list of all functions defined in the package' R code
    functions <- list_functions(file.path(package$path, "R"))
    if (verbose)
        cat(paste("Decorating",length(functions), "functions\n"))
    # capture all functions in the package
    for (f in functions) {
        decorate(f, package$package, verbose = verbose)
    }
    # start the capturing
    testr_options("capture.arguments", TRUE)
    # run package examples
    files <- devtools:::rd_files(package)
    if (verbose)
        cat(paste("Running examples (", length(files), "files)\n"))
    if (length(files) != 0)
        tryCatch(lapply(files, devtools:::run_example), error=function(x) print(x))
    # run package vignettes
    info <- tools:::getVignetteInfo(package = package$package)
    vdir <- info[,2]
    vfiles <- info[,6]
    p <- file.path(vdir, "doc", vfiles)
    if (verbose)
        cat(paste("Running vignettes (", length(vfiles), "files)\n"))
    # vignettes are not expected to be runnable, silence errors
    invisible(tryCatch(sapply(p, source), error=function(x) invisible()))
    if (include.tests) {
        if (verbose)
            cat("Running package tests\n")
        library(testthat, quietly = T)
        testthat:::run_tests(package.name)
    }
    # stop capturing
    testr_options("capture.arguments", FALSE)
    stop_capture_all()
    # generate the tests to the output directory
    if (missing(output)) {
        if (filter)
            output = "temp"
        else
            output = file.path(package$path, "tests")
    }
    if (verbose)
        cat(paste("Generating tests to", output, "\n"))
    generate(output, verbose = verbose)
    # filter, if enabled
    if (filter) {
        if (verbose)
            cat("Filtering tests - this may take some time...\n")
        filter_tests(output, file.path(package$path, "tests"), functions, package.dir, verbose = verbose)
    }
}
