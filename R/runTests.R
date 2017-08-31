

#' Runs all available source code from a package in order to generate
#' usable tests.
#'
#' @param pkg the name of the package (must be installed)
#' @param flist list of functions to instrument for capture
#' @param output.dir directory to write all harnes scripts, tests, and generated content.
run_package <- function(pkg, flist, output.dir = getwd(), validation.cache = new.env(), skip.existing = TRUE) {

    cat(sprintf("Package %s:\n", pkg))

    # Create a folder for test output, deleting it
    # if it already exists
    pkg.output.dir <- file.path(output.dir, "packages", pkg)
    if(dir.exists(pkg.output.dir)) {
        if(skip.existing) {
            cat(sprintf("  Already done, skipping.\n"))
            return(NULL)
        } else {
            unlink(pkg.output.dir, recursive = TRUE)
        }
    }
    dir.create(pkg.output.dir, recursive = TRUE)

    # Run package examples
    run_package_examples(pkg, flist, pkg.output.dir)

    # Run test scripts
    package.dir <- find.package(pkg)
    testScripts <- list.files(file.path(package.dir, "tests"), pattern = ".+\\.[RSrs]$", full.names = TRUE)

    for(script in testScripts) {
        run_package_source(pkg, flist, script, pkg.output.dir)
    }

    validate_tests(capture.dir = file.path(pkg.output.dir, "captured"),
                   validated.test.dir = file.path(output.dir, "validated"),
                   cache = validation.cache)
}

#' Executes a script in an independent R session and stores the output in the
#' package working directory.
#'
#' @param pkg the name of the package (must be installed)
#' @param source.file the full path of the source file to execute
#' @param flist list of functions to intrument
#' @param output.dir the output dir to write capture tests and output
run_package_examples <- function(pkg, flist, output.dir, validation.cache) {

    cat(sprintf("  Running Examples... "))

    script <- c(
        "library(testr)",
        sprintf("library(%s)", pkg),
        sprintf("setwd('%s')", output.dir),
        sprintf("start_capture(%s)", paste(deparse(flist), collapse="")),
        sprintf("example(%s)", pkg),
        sprintf("generate('%s')", file.path(output.dir, "captured"))
    )

    harnessScript <- file.path(output.dir, "examples.R")
    writeLines(script, harnessScript)

    scriptOutput = paste(harnessScript, "out", sep=".")
    errorCode <- run_script_with_timeout(harnessScript, scriptOutput)

    if(errorCode == 0) {
        cat("\n")
    } else {
        cat(sprintf("ERROR(%d)\n", errorCode))
    }
}


#' Executes a script in an independent R session and stores the output in the
#' package working directory.
#'
#' @param pkg the name of the package (must be installed)
#' @param source.file the full path of the source file to execute
#' @param flist list of functions to intrument
#' @param output.dir the output dir to write capture tests and output
run_package_source <- function(pkg, flist, source, output.dir) {
    cat(sprintf("  Running %s... ", basename(source)))

    script <- c(
        "library(testr)",
        sprintf("library(%s)", pkg),
        sprintf("setwd('%s')", dirname(source)),
        sprintf("start_capture(%s)", paste(deparse(flist), collapse="")),
        sprintf("source('%s', echo = TRUE)", basename(source)),
        sprintf("generate('%s')", file.path(output.dir, "captured"))
    )

    harnessScript <- file.path(output.dir, basename(source))
    writeLines(script, harnessScript)

    scriptOutput = paste(harnessScript, "out", sep=".")
    errorCode <- run_script_with_timeout(harnessScript, scriptOutput)

    if(errorCode == 0) {
        cat("\n")
    } else {
        cat(sprintf("ERROR(%d)\n", errorCode))
    }
}

#' Spawns a new VM to run the given script and write the output to the given file.
#'
#' If the script does not complete within 90 seconds, it is sent the the TERM signal.
#' If the process does not exit within 5 seconds, it is sent the KILL signal.
run_script_with_timeout <- function(script.file, output.file) {
    system2(command = "timeout",
            args = c("--kill-after=5s", "90s", "Rscript", script.file),
            stdout = output.file,
            stderr = output.file)
}

#' Attempts to run all generated tests to verify that they're actually correct.
#'
#'
validate_tests <- function(capture.dir, validated.test.dir, cache = new.env()) {

    cat(sprintf("  Validating tests...\n"))

    test.files <- list.files(capture.dir, pattern=".+\\.R", full.names = TRUE, recursive = TRUE)

    # Create the validated test dir if doesn't exist
    if(!dir.exists(validated.test.dir)) {
        dir.create(validated.test.dir)
    }

    ok <- 0
    total <- 0
    cacheHits <- 0
    for(test.file in test.files) {
        cacheKey <- basename(test.file)
        cached <- cache[[cacheKey]]
        if(!is.null(cached)) {
            cacheHits <- cacheHits + 1
        } else {

            if(file.size(test.file) > (1024 * 20)) {
                valid <- FALSE
            } else {
                test.output <- paste0(test.file, ".out")
                exitCode <- system2("timeout", args = c("2s", "Rscript", test.file),
                                    stdout = test.output,
                                    stderr = test.output)

                valid <- (exitCode == 0)
            }
            cache[[cacheKey]] <- valid

            if(valid) {
                file.copy(test.file, file.path(validated.test.dir, basename(test.file)))
                ok <- ok + 1
            }
            total <- total + 1
            if(total %% 500 == 0) {
                cat(sprintf("  Validated %d tests so far...\n", total))
            }
        }
    }

    cat(sprintf("  Validated %d/%d new tests, %d cached.\n", ok, (length(test.files)-cacheHits), cacheHits))
}



#' get_tests
#' @description get the names of all generated test cases
#' @param capt_dir location of test cases
#' @export
get_tests <- function(capt_dir)
{
    d <- list.files(capt_dir, pattern = ".R$", recursive = TRUE,
                    full.names = TRUE)
    rm <- grep("^capture", list.files(capt_dir, recursive = TRUE))
    if (length(rm))
        d <- d[-rm]
    d
}

#' generate_test_cases
#'
#' Systematically generates test cases for a given base function by running as many package
#' examples and tests as possible to capture inputs and outputs of functions. Or limited 
#' number of packages are used, if a 'limit' argument or 'MAX_PACKAGES_TO_RUN' environment 
#' variable is provided, 
#'
#' If the functions argument is missing, then the list of functions is read from the
#' environment variable FUNCTIONS
#'
#' @description generates test cases of base functions using package sources
#' @import devtools methods
#' @param functions Comma delimited names of functions to be decorated for test generation.
#' Provided as string vector or through environment variable FUNCTIONS.
#' @param limit max number of packages to be runned for test case generation. Provided as 
#' integer or through environment variable MAX_PACKAGES_TO_RUN. 
#' @export
generate_test_cases <- function(functions, limit)
{
    # Read from environment if not explicitly provided
    if(missing(functions)) {
        functions <- strsplit(Sys.getenv("FUNCTIONS"), split="[\\s,]+", perl = TRUE)[[1]]
        functions <- functions[ nzchar(functions) > 0 ]
        if(length(functions) == 0) {
            stop("No functions provided. Set the FUNCTIONS environment variable with a comma-delimited list of functions")
        }
    }
    if(missing(limit)) {
        limit <- strsplit(Sys.getenv("MAX_PACKAGES_TO_RUN"), split="[\\s,]+", perl = TRUE)[[1]]
        limit <- limit[ nzchar(limit) > 0 ]
        if(length(limit) == 0) {
            limit = 0
        } else {
            limit = as.integer(limit[1])
        }
    }

    cat(sprintf("function: %s\n", functions))
    if(limit) cat(sprintf("Number of packages to use: %s\n", limit))

    packages <- installed.packages()[, 1]
    if(limit) packages <- packages[1:limit]

    # Set up validation cache and output dir
    validation.cache <- new.env(hash = TRUE)

    for(pkg in packages) {
        run_package(pkg, functions, validation.cache = validation.cache)
    }
}

#' generate_test_cases_using
#' 
#' @description Generates test cases using the provided packages
#' @import devtools methods
#' @param functions list of functions to annotate for test case generation
#' @param packages list of packages to be used for test case generation
#' @export
generate_test_cases_using <- function(functions, packages)
{
    # Read from environment if not explicitly provided
    if(missing(functions)) {
        functions <- strsplit(Sys.getenv("FUNCTIONS"), split="[\\s,]+", perl = TRUE)[[1]]
        functions <- functions[ nzchar(functions) > 0 ]
        if(length(functions) == 0) {
            stop("No functions provided. Set the FUNCTIONS environment variable with a comma-delimited list of functions")
        }
    }
    if(missing(packages)) {
        packages <- strsplit(Sys.getenv("USE_PACKAGES"), split="[\\s,]+", perl = TRUE)[[1]]
        packages <- packages[ nzchar(packages) > 0 ]
        if(length(packages) == 0) {
            stop("No packages provided. Set the USE_PACKAGES environment variable with a comma-delimited list of package names")
        }
    }

    cat(sprintf("function(s): %s\n", functions))
    cat(sprintf("packages(s): %s\n", packages))

    # Set up validation cache and output dir
    validation.cache <- new.env(hash = TRUE)

    for(pkg in packages) {
        run_package(pkg, functions, validation.cache = validation.cache)
    }
}

