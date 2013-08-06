# Copyright (c) 2013, Purdue University. All rights reserved.
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
#
# This code is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 only, as
# published by the Free Software Foundation.
#
# This code is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# version 2 for more details (a copy is included in the LICENSE file that
# accompanied this code).
#
# You should have received a copy of the GNU General Public License version
# 2 along with this work; if not, write to the Free Software Foundation,
# Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
# or visit www.oracle.com if you need additional information or have any
# questions.

# target suite. This file is supposed to run on target



#' Launches the test suite. 
#' 
#' The root argument points to a folder that contains the R files to be executed. The R files are expected to be in the testR expanded format, that is each test has its source code, expected output (if output should be checked) and possible warnings and errors to be expected. 
#' 
#' The tests in all files are executed and reported textually.
#' 
#' Tests may have names which will be reported and used to identify the test. If the test has no name, its index in the actual tested file will be given as a name. 
#' 
#' @param root Folder where to look for the tests (recursively)
#' @param verbose If true, each test will be reported as it is executed.
#' @param summary If true, at the end of each file a summary will be given for each test in the file, showing its name, result and description of failure.
#' @param displayOnlyErrors If true, both verbose and summary printouts will only display failed tests. 
#' @param stopOnError If true, upon encountering first error, testR stops from executing further tests.
#' @param displayCodeOnError If true, for a failed test, its code will also be displayed. 
#' 
#' @return TRUE if all tests have passed, FALSE otherwise. 
#' 
#' @seealso test
#' 
#' @examples testSuite("c:/delete/tests", verbose = TRUE)

testSuite <- function(root, verbose = FALSE, summary = FALSE, displayOnlyErrors = FALSE, stopOnError = FALSE, displayCodeOnError = TRUE) {
    verbose <<- verbose
    displayOnlyErrors <<- displayOnlyErrors
    stopOnError <<- stopOnError
    displayCodeOnError <<- displayCodeOnError
    totalFails <- 0
    totalPasses <- 0
    if (verbose)
        cat(sprintf("%-80s", "Name"),"Result\n---------------------------------------------------------------------------------------\n")
    for (f in list.files(root, pattern=".[rR]$", recursive = TRUE)) {
        filename <- paste(root,"/", f, sep = "")
        cat(filename,"...\n")
        tests <<- list(c("Test Name","Result", "Comments"))
        fails <<- 0
        passes <<- 0
        source(filename, local = FALSE)
        cat("  (pass = ", passes,", fail = ", fails, ", total = ", passes + fails, ")\n", sep = "")
        totalFails <- totalFails + fails
        totalPasses <- totalPasses + passes
        if (summary == TRUE) {
            cat(sprintf("%-80s", "Name"),"Result\n---------------------------------------------------------------------------------------\n")
            for (t in tests[-1])
                print.test(t)
            cat("\n")
        }
    }
    rm(tests, envir = globalenv())
    rm(fails, envir = globalenv())
    rm(passes, envir = globalenv())
    rm(verbose, envir = globalenv())
    rm(displayOnlyErrors, envir = globalenv())
    rm(stopOnError, envir = globalenv())
    rm(displayCodeOnError, envir = globalenv())
    cat("\n-------- Results --------\n")
    cat("Passed:   ", totalPasses, "\n")
    cat("Failed:   ", totalFails, "\n")
    cat("Total:    ", totalPasses + totalFails, "\n")
    if (totalFails == 0) {
        cat("Overall:  ", "PASS\n")
        TRUE
    } else {
        cat("Overall:  ", "FAIL\n")
        FALSE
    }
} 

#' Prettyprints the test, is intended to be used only internally. 
print.test <- function(test, code = NULL) {
    if (displayOnlyErrors && identical(test[[2]], "PASS"))
        return()
    if (nchar(test[[1]]) > 80)
        test[[1]] <- paste("...",substr(test[[1]], length(test[[1]])-77, length(test[[1]])), sep = "")
    cat(sprintf("%-80s", test[[1]]),test[[2]],"\n")
    if (! identical(test[[2]], "PASS")) {
        cat(" ",test[[3]], "\n")
        if (displayCodeOnError && ! is.null(code)) {
            cat("  Code:\n")
            for (l in deparse(code))
                cat("    ", l, "\n", sep="")
        }
    }
}

compareResults <- function(a, b) {
    if (identical(all.equal(a, b), TRUE)) {
        TRUE
    } else if (is.na(a) && is.na(b) && !is.nan(a) && (!is.nan(b))) {
        TRUE # we do not care about types of NA's, or should we -- I think this should be tested by a test rather than assumed here
#    } else if (typeof(a) != typeof(b)) {
#        FALSE
#    } else if (typeof(a) == "double") {
#        
#    }
#    if (is.na(a) && (is.na(b))) {
#        TRUE
    } else {
        FALSE        
    }
}

#' Creates a test and evaluates its result. 
#' 
#' The test is a success if the expected output is identical to the actual output and expected (or none) warnings have been reported during the execution, or if the code itself failed and the expected error has been found.
#' 
#' Note that this function is not intended to be directly executed by user. Use the testSuite function on how to run the tests.
#' 
#' @param code The code of the test, must be a runnable R code.
#' @param output Output of the test, if not specified no output will be checked (in case of an error expected)
#' @param expectWarning String to find in the warning messages (scalar or vector)
#' @param expectError String to find in the errro messages (scalar or vector)
#' @param name Name of the test. If no name is specified, the index of the test in the file will be used.
#' 
#' @returns TRUE if the test passes, FALSE otherwise
#'
#' @seealso testSuite
#' 
#' @examples 
#' test({
#'   a = 1
#'   b = 2
#'   a + b
#'   }, 2, name = "simple test")
#' 
#' test( {
#'   warning("example warning")
#'   TRUE
#'   }, TRUE, expectWarning = "warning", name = "warning example)

#' test( {
#'   stop("error")
#'   TRUE
#'   }, expectError = "error", name = "error example)
#' 
test <- function(code, output = NULL, expectWarning = NULL, expectError = NULL, name = NULL) {
    appendComment <- function(...) {
        s <- list(...)[[1]]
        for (o in list(...)[-1])
            s <- paste(s, paste(o, collapse = " "))
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
        tryCatch(
            eval(code, envir = new.env(parent=baseenv())),
            error = function(e) {
                errors <<- e$message
            }),
        warning = function(w) {
            if (is.null(warnings))
                warnings <<- w$message
            else 
                warnings <<- paste(warnings, w$message, sep = "; ")
            invokeRestart("muffleWarning")
        }
    )
    # if we have an error, the result is irrelevant and should be NULL
    if (!is.null(errors))
        result <- NULL
    if (compareResults(result, output)) {
        result <- "PASS"
    } else {
        appendComment("Expected",output, "got", result)
        result <- "FAIL"
    }
    # check the warnings
    if (missing(expectWarning)) {
        if (!is.null(warnings)) {
            result <- "FAIL"
            appendComment("Expected no warnings, but", warnings,"found")
        }
    } else {
        for (w in expectWarning) 
            if (length(grep(w, warnings)) == 0) {
                result <- "FAIL"
                appendComment("Warning", w, "not found in", warnings)
            }
    }
    # check the errors
    if (missing(expectError)) {
        if (!is.null(errors)) {
            result <- "FAIL"
            appendComment("Expected no errors, but", errors,"found")
        }
    } else {
        for (e in expectError) 
            if (length(grep(e, errors)) == 0) {
                result <- "FAIL"
                appendComment("Error", e, "not found in ", errors)
            }
    }
    if (missing(name))
        name <- as.character(length(tests))
    tests[[length(tests) + 1]] <<- c(name, result, comments)
    if (verbose)
        print.test(tests[[length(tests)]], code)
    if (identical(result, "PASS")) {
        passes <<- passes + 1
        TRUE
    } else {
        fails <<- fails + 1
        if (stopOnError) {
            stop("Test ",name," failed: ", comments)
        }
        FALSE
    }
}
