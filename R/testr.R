#' Adds regression tests to specified package.
#'
#' @description Given a package, the code to be executed and a list of functions to capture, the function captures the selected functions from the package, then runs the specified code. It then generates tests from the captured information and using code coverage filters them against existing tests for the package. Those that increase the code coverage will be added to already existing testthat tests for the package.
#'
#' @param package.dir Name/path to the package, uses devtools notation
#' @param code Function (with no arguments) whose code will be executed and its calls in the package captured.
#' @param functions Functions from the package to be captured, if missing, all package functions will be captured (character vector)
#' @param filter T if the generated tests should be filtered
#' @param exclude_existing_tests If TRUE, existing tests will be ignored from the code coverage
#' @param build T if the package will be built beforehand
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if not specified, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export

gen_from_function <- function(package.dir = ".", code, functions, filter = TRUE,
                              exclude_existing_tests = FALSE, build = TRUE,
                              timed = FALSE, output,
                              verbose = testr_options("verbose"))
{
    cleanup = FALSE
    # stop all ongoing captures
    stop_capture_all()
    if (build) {
        if (verbose)
            cat(paste("Building package", package.dir, "\n"))
        package.path = devtools::build(package.dir, quiet = ! verbose)
        if (verbose)
            cat(paste("  built into", package.dir, "\n"))
    } else {
        package.path = package.dir
    }
    # install the package
    if (verbose) cat(paste("Installing package", package.path, "\n"))
    install.packages(package.path, repos = NULL, quiet = TRUE, type = "source")
    # get list of all functions
    package = devtools::as.package(package.dir)
    # TODO I don't thing this line is needed anymore
    l <- library
    l(package = package$package, character.only = TRUE)
    if (verbose) cat(paste("Package", package$package, "installed\n"))
    # if function names were not specified,
    if (missing(functions)) {
        # get list of all functions defined in the package' R code
        functions <- list_functions(file.path(package$path, "R"))
        if (verbose) cat("All functions from package will be decorated\n")
    }
    if (verbose) cat(paste("Decorating",length(functions), "functions\n"))
    # capture all functions in the package
    for (f in functions) {
        decorate(f, package$package, verbose = verbose)
    }
    # start the capturing
    testr_options("capture.arguments", TRUE)
    # run the code
    code()
    # stop capturing
    testr_options("capture.arguments", FALSE)
    stop_capture_all()
    # generate the tests to the output directory
    if (missing(output)) {
        if (filter) {
            output = "temp"
            cleanup = TRUE
        } else {
            output = file.path(package$path, "tests")
        }
    }
    if (verbose)
        cat(paste("Generating tests to", output, "\n"))
    generate(output, verbose = verbose)
    # filter, if enabled
    if (filter) {
        if (verbose)
            cat("Pruning tests - this may take some time...\n")
        filter_tests(output, file.path(package$path, "tests/testthat"),
                     functions, package.dir, compact = TRUE, verbose = verbose)
    }
    # clear the temp folder, if we used a temp folder implicitly
    if (cleanup) unlink(output, recursive = T)
    detach(paste("package", package$package, sep=":"), unload = TRUE,
           character.only = TRUE)
}

#' Generates tests for a package by running the code associated with it.
#'
#' @description Runs the examples, vignettes and possibly tests associated with the package and captures the usage of package's functions. Creates tests from the captured information, filters it according to the already existing tests and if any new tests are found, adds them to package's tests.
#'
#' @param package.dir Name/path to the package, uses devtools notation.
#' @param include.tests If TRUE, captures also execution of package's tests.
#' @param build if to build package before. Default \code{TRUE}
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param filter TRUE if generated tests should be filteres so that only those adding to a coverage will be used
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if not specified, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export
#'
gen_from_package <- function(package.dir = ".", include.tests = FALSE,
                             timed = FALSE, filter = TRUE, build = TRUE,
                             output, verbose = testr_options("verbose"))
{
    package = devtools::as.package(package.dir)
    devtools::document(package.dir)
    detach(paste("package", package$package, sep=":"), unload = TRUE,
           character.only = TRUE)
    f <- function() {
        # run package vignettes
        info <- tools::getVignetteInfo(package = package$package)
        vdir <- info[ ,2]
        vfiles <- info[ ,6]
        p <- file.path(vdir, "doc", vfiles)
        if (verbose) cat(paste("Running vignettes (", length(vfiles), "files)\n"))
        # vignettes are not expected to be runnable, silence errors
        invisible( tryCatch( sapply(p, source), error = function(x) invisible() ) )
        # run package examples
        manPath <- file.path(package.dir, "man")
        examples <- list.files(manPath, pattern = "\\.[Rr]d$", no.. = TRUE)
        if (length(examples) != 0) {
            if (verbose)
                cat(paste("Running examples (", length(examples), "man files)\n"))
            for (f in examples) {
                code <- example_code(file.path(manPath, f))
                tryCatch(eval(parse(text = code)), error = function(x) print(x))
            }
        }
        # run tests
        if (include.tests) {
            if (verbose) cat("Running package tests\n")
            testthat::test_dir(file.path(package.dir, "tests", "testthat"),
                               filter = NULL)
        }
    }
    if (missing(output))
        gen_from_function(package.dir, code = f , filter = filter,
                          exclude_existing_tests = include.tests, build = build,
                          timed = timed, verbose = verbose)
    else
        gen_from_function(package.dir, code = f , filter = filter,
                          exclude_existing_tests = include.tests, build = build,
                          timed = timed, output, verbose = verbose)
}

#' Enables capturing of the specified functions.
#'
#' @description The functions can be expressed as character literals in the form of either only a function name,
#' or package name ::: function name, or directly as a symbol (i.e. only function name, or package name ::: function name).
#'
#' @param ... List of functions to capture, either character literals, or symbols
#' @param verbose TRUE to display additional information
#' @export
start_capture <- function(..., verbose = testr_options("verbose"))
{
    old <- testr_options("capture.arguments")
    if (old) testr_options("capture.arguments", FALSE)
    for (f in parseFunctionNames(...)) {
        decorate(f[["name"]], f[["package"]], verbose = verbose)
    }
    if (old) testr_options("capture.arguments", TRUE)
    invisible(NULL)
}

#' Enables capturing of all builtin functions.
#'
#' @description Wrapper around capture to capture builtin functions
#' @param internal.only TRUE if only internal functions should be captured
#' @param verbose TRUE to display additional information
#' @export
start_capture_builtins <- function(internal.only = FALSE,
                                   verbose = testr_options("verbose"))
{
    functions <- builtins(internal.only)
    setup_capture(functions, verbose = verbose)
}

#' Stops capturing the selected functions.
#'
#' @description This function removes the tracing functionality for specified function
#' @param ... Functions whose capture is to be dropped (uses the same format as capture)
#' @param verbose TRUE to display additional information
#' @export
stop_capture <- function(..., verbose = testr_options("verbose"))
{
    for (f in parseFunctionNames(...)) {
        undecorate(f$name, f$package, verbose = verbose)
    }
    invisible(NULL)
}

#' Stops capturing all currently captured functions.
#'
#' @description Remove tracing functionality for all the functions
#' @param verbose TRUE to display additional information
#' @export
stop_capture_all <- function(verbose = testr_options("verbose"))
{
    clear_decoration(verbose = verbose)
}

#' Generates tests from captured information.
#'
#' @description This function takes the tracing information collected by capture and generates
#' testthat compatible testcases.
#'
#' @param output_dir Directory to which the tests should be generated.
#' @param root Directory with the capture information, defaults to capture.
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param verbose TRUE to display additional information.
#' @param clear_capture if FALSE captured traces will not be deleted after the generation so that subsequent calls to generate() can use them too
#' @export
generate <- function(output_dir, root = testr_options("capture.folder"),
                     timed = FALSE, clear_capture = TRUE,
                     verbose = testr_options("verbose"))
{
    cache$output.dir <- output_dir
    test_gen(root, output_dir, timed, verbose = verbose);
    if (clear_capture) {
        unlink(file.path(root, list.files(path = root, no.. = TRUE)))
    }

}

#' Filter the generated tests so that only tests increasing code coverage will be kept.
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
#' @param compact If TRUE, the filtered tests will be compacted into files one per function, rather than the default one per test.
#' @param verbose whether the additional information should be displayed. Default: \code{TRUE}
#' @return NULL
#'
#' @export
prune <- function(test_root, output_dir, ...,
                   package_path = "", remove_tests = FALSE, compact = FALSE,
                   verbose = testr_options("verbose"))
{
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
    filter_tests(test_root, output_dir, functions, package_path,
                 remove_tests = remove_tests, compact = compact,
                 verbose = verbose)
    invisible(NULL)
}

# helpers -------------------------------------------------------------------------------------------------------------

# TODO these are also in evaluation.R, perhaps evaluation.R should die and its non-api should move to helpers, or someplace else?

#' Generate tests for give code
#' @description Generates tests from given code and specific captured functions
#'
#' @param code Code from which the tests will be generated.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... functions to be captured during the code execution (same syntax as capture function)
#' @export
gen_from_code <- function(code, output_dir, ...)
{
    code <- substitute(code)
    start_capture(...)
    eval(code)
    stop_capture_all()
    generate(output_dir)
    invisible()
}

#' Generate tests for give source
#' @description Generates tests by running given source file.
#'
#' @param src.root Source file to be executed and captured, or directory containing multiple files.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... Functions to be tested.
#' @export
gen_from_source <- function(src.root, output_dir, ...)
{
    if (!file.exists(src.root)) {
        stop("Supplied source does not exist")
    }
    if (file.info(src.root)$isdir) {
        src.root <- list.files(src.root, pattern = "\\[rR]",
                               recursive = TRUE, full.names = TRUE)
    }
    start_capture(...)
    for (src.file in src.root) {
        source(src.file, local = TRUE)
    }
    stop_capture_all()
    generate(output_dir)
    invisible()
}
