#' @title Adds regression tests to specified package.
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

testr_addRegression <- function(package.dir = ".", code, functions, filter = TRUE, exclude_existing_tests = FALSE, build = TRUE, timed = FALSE, output, verbose = testr_options("verbose")) {
    cleanup = F
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
    if (verbose)
        cat(paste("Installing package", package.path, "\n"))
    #devtools:::install(package.name, quiet = T)
    install.packages(package.path, repos = NULL, quiet = T, type = "source")
    # get list of all functions
    package = devtools::as.package(package.dir)
    # TODO I don't thing this line is needed anymore
    l <- library
    l(package = package$package, character.only = T)
    if (verbose)
        cat(paste("Package", package$package, "installed\n"))
    # if function names were not specified,
    if (missing(functions)) {
        # get list of all functions defined in the package' R code
        functions <- list_functions(file.path(package$path, "R"))
        if (verbose)
            cat(paste("All functions from package will be decorated"))
    }
    if (verbose)
        cat(paste("Decorating",length(functions), "functions\n"))
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
            cleanup = T
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
            cat("Filtering tests - this may take some time...\n")
        filter_tests(output, file.path(package$path, "tests/testthat"), functions, package.dir, compact = T, verbose = verbose)
    }
    # clear the temp folder, if we used a temp folder implicitly
    if (cleanup)
        unlink(output, recursive = T)
    detach(paste("package", package$package, sep=":"), unload = T, character.only = T)
}



#' @title Generates tests for a package by running the code associated with it.
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
testr_package <- function(package.dir = ".", include.tests = FALSE, timed = FALSE, filter = TRUE, build = TRUE, output, verbose = testr_options("verbose")) {
    package = devtools::as.package(package.dir)
    devtools::document(package.dir)
    detach(paste("package", package$package, sep=":"), unload = T, character.only = T)
    f <- function() {
        # run package vignettes
        info <- tools::getVignetteInfo(package = package$package)
        vdir <- info[,2]
        vfiles <- info[,6]
        p <- file.path(vdir, "doc", vfiles)
        if (verbose)
            cat(paste("Running vignettes (", length(vfiles), "files)\n"))
        # vignettes are not expected to be runnable, silence errors
        invisible(tryCatch(sapply(p, source), error=function(x) invisible()))
        # run package examples
        manPath <- file.path(package.dir, "man")
        examples <- list.files(manPath, pattern = "\\.[Rr]d$", no.. = T)
        if (length(examples) != 0) {
            if (verbose)
                cat(paste("Running examples (", length(examples), "man files)\n"))
            for (f in examples) {
                code <- example_code(file.path(manPath, f))
                tryCatch(eval(parse(text = code)), error=function(x) print(x))
            }
        }
        # run tests
        if (include.tests) {
            if (verbose)
                cat("Running package tests\n")
            testthat::test_dir(file.path(package.dir, "tests", "testthat"), filter = NULL)
        }
    }
    if (missing(output))
        testr_addRegression(package.dir, code = f , filter = filter, exclude_existing_tests = include.tests, build = build, timed = timed, verbose = verbose)
    else
        testr_addRegression(package.dir, code = f , filter = filter, exclude_existing_tests = include.tests, build = build, timed = timed, output, verbose = verbose)
}









#' @title Enables capturing of the specified functions.
#'
#' @description The functions can be expressed as character literals in the form of either only a function name,
#' or package name ::: function name, or directly as a symbol (i.e. only function name, or package name ::: function name).
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
#' @description Wrapper around capture to capture builtin functions
#' @param internal.only TRUE if only internal functions should be captured
#' @param verbose TRUE to display additional information
#' @export
capture_builtins <- function(internal.only = FALSE, verbose = testr_options("verbose")) {
    functions <- builtins(internal.only)
    setup_capture(functions, verbose = verbose)
}

#' @title Stops capturing the selected functions.
#'
#' @description This function removes the tracing functionality for specified function
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
#' @description Remove tracing functionality for all the functions
#' @param verbose TRUE to display additional information
#' @export
stop_capture_all <- function(verbose = testr_options("verbose")) {
    clear_decoration(verbose = verbose)
}

#' @title Generates tests from captured information.
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
                     timed = F, clear_capture = T, verbose = testr_options("verbose")) {
    cache$output.dir <- output_dir
    test_gen(root, output_dir, timed, verbose = verbose);
    if (clear_capture) {
        unlink(file.path(root, list.files(path = root, no.. = T)))
    }

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
#' @param compact If TRUE, the filtered tests will be compacted into files one per function, rather than the default one per test.
#' @param verbose whether the additional information should be displayed. Default: \code{TRUE}
#' @return NULL
#'
#' @export
filter <- function(test_root, output_dir, ...,
                   package_path = "", remove_tests = FALSE, compact = FALSE,
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
    filter_tests(test_root, output_dir, functions, package_path, remove_test = remove_tests, compact = compact, verbose = verbose)
    invisible(NULL)
}

#' @title Runs the generated tests.
#'
#' @description This function is a shorthand for calling testthat on the previously generated tests.
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

#' @title Generate tests for give code
#' @description Generates tests from given code and specific captured functions
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

#' @title Generate tests for give source
#' @description Generates tests by running given source file.
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

#' @title Capture run information from CRAN packages
#'
#' @description This function is responsible for getting all possible capture information from CRAN
#' It tries to insstall package and run tests, examples and vignettes
#' @param name name of the package to be downloaded
#' @param dir resulting directory with tests
#' @param indexes indexes of specific packages
#' @param funcs functions to Decorate
#' @export
#'
cran_gen <- function(name, dir, funcs=NULL, indexes = 1:10000) {
  if (!missing(name)) {
    ap <- name
  } else {
    ap <- available.packages()[indexes,1]
  }
  sapply(ap, package_gen, gen.dir = dir, funcs = funcs)
}

#' @title Capture run information from Bioconductor packages
#'
#' @description This function is responsible for getting all possible capture information from Bioconductor
#' It tries to insstall package and run tests, examples and vignettes
#' @param name name of the package to be downloaded
#' @param dir resulting directory with tests
#' @param indexes indexes of specific packages
#' @param funcs functions to Decorate
#' @export
#'
bioconductor_gen <- function(name, dir, funcs=NULL, indexes = 1:1000) {
  contriburl <- paste(biocinstallRepos()["BioCsoft"], "src/contrib", #nolint
                      sep = "/")
  if (!missing(name)) {
    ap <- name
  } else {
    ap <- available.packages(contriburl)[indexes,1]
  }
  sapply(ap, package_gen, from.bioc = T, contriburl = contriburl, funcs = funcs, gen.dir = dir)
}

#' @title Capture run information from package and generate test cases
#'
#' @description This function is responsible for getting all possible capture information from specific package.
#' It tries to insstall package and run tests, examples and vignettes
#' @param name name of the package
#' @param gen.dir resulting directory with tests
#' @param from.bioc if package is from Bioconductior
#' @param contriburl contributor url as in download.packages
#' @param funcs functions to Decorate
#' @param gen if generate test cases
#' @export
#'
package_gen <- function(name, gen.dir, funcs, from.bioc = FALSE, contriburl) {
  dir <- tempdir()
  if (!missing(contriburl)) {
    loc <- suppressMessages(download.packages(name, dir, contriburl = contriburl, type = "source")[,2])
  } else {
    loc <- suppressMessages(download.packages(name, dir, type = "source")[,2])
  }
  untar(loc, exdir = dir)
  if (!missing(dir)) file.remove(loc)
  loc <- file.path(dir, name)
  cat("===Intalling package - ", name, "\n")
  if (name %in% loadedNamespaces())
    tryCatch(detach(name=paste("package", name, sep=":"),unload = T, character.only = T),
             error=function(x) invisible())
  if (from.bioc) {
    biocLite(name) #nolint
  } else {
    install.packages(name, quiet = T, dependencies = T)
  }
  if (!do.call(require, list(name))) {
    cat("===Package Loading Failed\n")
    return(invisible())
  }
  if (!file.exists(gen.dir) || !file.info(gen.dir)$isdir) dir.create(gen.dir)
  cat("===Inserting Trace points\n")
  if (missing(funcs) || is.null(funcs)) {
    funcs <- ls(getNamespace(name))
    setup_capture(funcs, name)
  } else {
    setup_capture(funcs)
  }
  hs <- help
  inv <- function(x) invisible()
  reassing_in_env("help", inv, getNamespace("utils"))
  reassing_in_env("help", inv, as.environment("package:utils"))

  cat("===Running examples\n")
  capture.output(run_examples(loc))
  cat("===Running tests\n")
  capture.output(run_tests(loc))
  cat("===Running vignettes\n")
  capture.output(run_vignettes(name))

  reassing_in_env("help", hs, getNamespace("utils"))
  reassing_in_env("help", hs, as.environment("package:utils"))
  cat("===Removing trace points\n")
  clear_decoration()
  cat("===Generating tests\n")
  TestGen("capture", file.path(gen.dir, name))
  file.remove(list.files("capture", recursive = T, full.names = T))
  invisible()
}
