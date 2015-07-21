#' @export
#' @title Determines if given function is a proper testlisterenr listener or not. 
#' 
#' A test listener is a function that has exactly five arguments named "id", "name", "result", "filename" and "comments" in this order. The id is the unique id of the test within the run_tests, name is the name of the test. Result is TRUE for passed test, or FALSE for a failed one. 
#' 
#' @param f Function to check
#' @return TRUE if the function has the valid signature, FALSE otherwise
#' @seealso run_tests
#' 
is_test_listener <- function(f) {
    identical(names(formals(f)), c("id", "name", "result", "filename", "comments"))
}

#' @export
#' @title Launches the test suite on the target VM. 
#' 
#' run_tests takes as an argument the root folder where the expanded test files are stored and then launches all tests found in all R (.r or .R) files recursively found in that location. Each test is executed and its output checked. Based on the optional arguments, different reporting methods can be used. 
#' 
#' By default, a text output is given on the standard output summarizing the numbers of failed / passed tests. If a listener function is provided, each analyzed test will invoke a call of this function so that more detailed reporting can be implemented directly by the caller. 
#' 
#' Note that since the run_tests method also runs on the tested VM, the VM must at least support the functionality required by this function (and other functions used for the test analysis). 
#' 
#' @param root Folder where to recursively look for the expanded tests. The tests must be located in files with extension either r or R. All other files are ignored.
#' @param verbose If TRUE, each test will be reported to the stdout as soon as it was executed and analyzed.
#' @param file.summary If TRUE, at the end of each analyzed file, a summary of all its tests will be printed. The summary contains the name and id of the test, its result and possibly comments to the reason for its failure.
#' @param display_only_errors If TRUE, in either verbose mode, or summary, detailed information will only be printed about failed tests.
#' @param stop_on_error If TRUE, first failed test will also terminate the execution of the test suite.
#' @param display_code_on_error if TRUE, the code of the test will be displayed when the test fails (only relevant for verbose mode). To determine the code, deparse function is used.
#' @param test_listener A function that will be called for each test. If supplied, it must be a proper test listener function whuich is checked by the is.test_listener function (a proper test listener function has exactly five arguments, id, name, result, filename and comments).
#'  
#' @return TRUE if all tests have passed, FALSE otherwise. 
#' 
#' @seealso test, is_test_listener

run_tests <- function(root,
                      verbose = testr_options("verbose"),
                      file.summary = testr_options("file.summary"),
                      display_only_errors = testr_options("display_only_errors"),
                      stop_on_error = testr_options("stop_on_error"),
                      display_code_on_error = testr_options("display_code_on_error"),
                      test_listener = NULL,
                      clean.wd = FALSE,
                      use.rcov = FALSE) {
    if (!missing(test_listener))
        if (!is_test_listener(test_listener))
            stop("Invalid function supported as a test listener.")
    total_fails <- 0
    total_passes <- 0
    # remember files in working directory, not to delete them after
    if (clean.wd)
        files_before_fun_wd <- list.files(getwd(), all.files = TRUE) # to clean R Working Directory
    # cache parameters to use them in tests
    cache$verbose <- verbose
    cache$display_only_errors <- display_only_errors
    cache$stop_on_error <- stop_on_error
    cache$display_code_on_error <- display_code_on_error
    cache$use.rcov <- use.rcov
    if (!file.exists(root) || is.null(root))
        return(NULL)
    # make list of test to run
    if (file.info(root)$isdir){
        files <- list.files(root, pattern=".[rR]$", recursive = TRUE, all.files = TRUE)
        files <- sapply(files, function (x) paste(root,"/",x, sep=""))
    } else {
        files <- root
    }
    if (verbose)
        cat(sprintf("%-80s", "Name"),
            "Result\n---------------------------------------------------------------------------------------\n")
    # process every file
    for (filename in files) {
        cat(filename,"...\n")
        cache$tests <- list(c("Test Name","Result", "Comments", "Id"))
        cache$fails <- 0
        cache$passes <- 0
        source(filename, local = FALSE)
        # invoke the test listener so that the results can be grabbed
        if (!is.null(test_listener))
            for (t in cache$tests[-1])
                test_listener(t[[4]], t[[1]], t[[2]], filename, t[[3]])
        cat("  (pass = ", cache$passes,
            ", fail = ", cache$fails,
            ", total = ", cache$passes + cache$fails,
            ")\n", sep = "")
        total_fails <- total_fails + cache$fails
        total_passes <- total_passes + cache$passes
        if (file.summary == TRUE) {
            cat(sprintf("%-80s", "Name"),
                "Result\n---------------------------------------------------------------------------------------\n")
            for (t in cache$tests[-1])
                print_tests(t)
            cat("\n")
        }
    }
    # summarized results
    cat("\n-------- Results --------\n")
    cat("Passed:   ", total_passes, "\n")
    cat("Failed:   ", total_fails, "\n")
    cat("Total:    ", total_passes + total_fails, "\n")
    if (total_fails == 0) {
        cat("Overall:  ", "PASS\n")
        return(TRUE)
    } else {
        cat("Overall:  ", "FAIL\n")
        return(FALSE)
    }
    # clean working directory
    if (clean.wd){
        files_after_filter_wd <- list.files(getwd(), all.files = TRUE)
        files_to_delete <- files_after_filter_wd[ ! (files_before_fun_wd %in% files_after_filter_wd)]
        file.remove(files_to_delete)
    }
    # clean cache
    rm(tests, envir = cache) #nolint
    rm(fails, envir = cache) #nolint
    rm(passes, envir = cache) #nolint
    rm(verbose, envir = cache)
    rm(display_only_errors, cache)
    rm(stop_on_error, envir = cache)
    rm(display_code_on_error, envir = cache)
}

#" Prettyprints the test, is intended to be used only internally. 
print_tests <- function(test, code = NULL) {
    display_only_errors <- ifelse(!is.null(cache$display_only_errors),
                                  cache$display_only_errors,
                                  testr_options("display_only_errors"))
    display_code_on_error <- ifelse(!is.null(cache$display_code_on_error),
                                    cache$display_code_on_error,
                                    testr_options("display_code_on_error"))
    if (display_only_errors && identical(test[[2]], "PASS"))
        return()
    if (nchar(test[[1]]) > 80)
        test[[1]] <- paste("...",
                           substr(test[[1]], length(test[[1]]) - 77, length(test[[1]])),
                           sep = "")
    cat(sprintf("%-80s", test[[1]]),if (test[[2]]) "PASS" else "FAIL","\n")
    if (test[[2]] == FALSE) {
        cat(" ",test[[3]], "\n")
        if (display_code_on_error && ! missing(code)) {
            cat("  Code:\n")
            for (l in deparse(code))
                cat("    ", l, "\n", sep="")
        }
    }
}

#' Comparing the results, also only to be used internally
compare_results <- function(a, b) {
    if(is.language(a))
        a <- as.expression(a)
    if (identical(all.equal(a, b), TRUE) || identical(as.expression(a), as.expression(b))) {
        return(TRUE)
    } else if (identical(all.equal(is.na(a),is.na(b)), TRUE))  {
        # special case for builtin/closure/special
        # because is.na produces warnings on them and equality would have been caught earlier,
        # so if all.equal previously did not return TRUE, comparasion is wrong
        if ((typeof(a) %in% c("builtin", "special", "closure", "symbol"))
            || (typeof(b) %in% c("builtin", "special", "closure", "symbol")))
            return(FALSE)
        # remove NA"s
        aa <- a[!is.na(a)]
        bb <- b[!is.na(b)]
        return ((length(aa) == 0) && (length(bb) == 0)) || identical(all.equal(aa, bb), TRUE)
    } else {
        return(FALSE)
    }
}

#' Creates a test and evaluates its result. 
#' 
#' The test is a success if the expected output is identical to the actual output and expected (or none) warnings have been reported during the execution, or if the code itself failed and the expected error has been found. Two NA values are always identical regardless their type. 
#' 
#' This function effectively defines the test and should be used in the test files. However, the test should only be executed by the run_tests function which also prepares the necessary environment for the test function. 
#' 
#' @param id the unique id of the test in the run_tests. 
#' @param code The code of the test, must be a runnable R code.
#' @param o Output of the test, if not specified no output will be checked (in case of an error expected)
#' @param w String to find in the warning messages (scalar or vector)
#' @param e String to find in the error messages (scalar or vector)
#' @param name Name of the test. If no name is specified, the index of the test in the file will be used.
#' 
#' @return TRUE if the test passes, FALSE otherwise
#'
#' @seealso run_tests
#' @export
test <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
    tests <- ifelse(!is.null(cache$tests), cache$tests, list(c("Test Name","Result", "Comments", "Id")))
    fails <- ifelse(!is.null(cache$fails), cache$fails, 0)
    passes <- ifelse(!is.null(cache$passes), cache$passes, 0)
    verbose <- ifelse(!is.null(cache$verbose),cache$verbose, testr_options("verbose"))
    stop_on_error <- ifelse(!is.null(cache$stop_on_error),cache$stop_on_error, testr_options("stop_on_error"))

    append_comment <- function(...) {
        s <- list(...)[[1]]
        for (o in list(...)[-1])
            s <- paste(s, paste(deparse(o), collapse = " "))
        if (is.null(comments))
            comments <<- s
        else
            comments <<- paste(comments, s, sep = ". ")
    }
    warnings <- NULL
    errors <- NULL
    code <- substitute(code)
    comments <- NULL
    # execute the test and grap warnings and errors
    result <- withCallingHandlers(
        tryCatch({
            if (cache$use.rcov) rcov::ResumeMonitorCoverage()
            res <- eval(code, envir = new.env())
            if (cache$use.rcov) rcov::PauseMonitorCoverage()
            res
        },
        error = function(e) errors <<- e$message, silent = TRUE),
        warning = function(w) {
            if (is.null(warnings)) {
                warnings <<- w$message
            } else {
                warnings <<- paste(warnings, w$message, sep = "; ")
            }
            invokeRestart("muffleWarning")
        })

    # if we have an error, the result is irrelevant and should be NULL
    if (!is.null(errors)) {
        result <- TRUE
    } else if (compare_results(result, o)) {
        result <- TRUE
    } else {
        append_comment("Expected",o, "got", result)
        result <- FALSE
    }
    # check the warnings
    if (missing(w)) {
        if (!is.null(w)) {
            result <- FALSE
            append_comment("Expected no warnings, but", warnings,"found")
        }
    } else {
        for (ww in w) {
            if (length(grep(ww, warnings)) == 0) {
                result <- FALSE
                append_comment("Warning", ww, "not found in", warnings)
            }
        }
    }
    # check the errors
    if (missing(e)) {
        if (!is.null(errors)) {
            result <- FALSE
            append_comment("Expected no errors, but", errors,"found")
        }
    } else {
        for (ee in e)
            if (length(grep(ee, errors)) == 0) {
                result <- FALSE
                append_comment("Error", ee, "not found in ", errors)
            }
    }
    if (missing(name))
        name <- as.character(length(tests))
    tests[[length(tests) + 1]] <- c(paste("[",id,"] ",name, sep = ""), result, comments, id)
    if (verbose)
        print_tests(tests[[length(tests)]], code)
    if (result) {
        passes <- passes + 1
        rv <- TRUE
    } else {
        fails <- fails + 1
        if (stop_on_error) {
            stop("Test id ", id, " name " ,name," failed: ", comments)
        }
        rv <- FALSE
    }
    if (!is.null(cache$tests)){
        cache$tests <- tests
        cache$passes <- passes
        cache$fails <- fails
    }
    return (rv)
}
