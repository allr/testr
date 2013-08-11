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

#' @export
#' Determines if given function is a proper testlisterenr listener or not. 
#' 
#' A test listener is a function that has exactly five arguments named "id", "name", "result", "filename" and "comments" in this order. The id is the unique id of the test within the runTests, name is the name of the test. Result is TRUE for passed test, or FALSE for a failed one. 
#' 
#' @param f Function to check
#' @return TRUE if the function has the valid signature, FALSE otherwise
#' @seealso runTests
#' 
is.testListener <- function(f) {
    identical(names(formals(f)), c("id", "name", "result", "filename", "comments"))
}

#' @export
#' Launches the test suite on the target VM. 
#' 
#' runTests takes as an argument the root folder where the expanded test files are stored and then launches all tests found in all R (.r or .R) files recursively found in that location. Each test is executed and its output checked. Based on the optional arguments, different reporting methods can be used. 
#' 
#' By default, a text output is given on the standard output summarizing the numbers of failed / passed tests. If a listener function is provided, each analyzed test will invoke a call of this function so that more detailed reporting can be implemented directly by the caller. 
#' 
#' Note that since the runTests method also runs on the tested VM, the VM must at least support the functionality required by this function (and other functions used for the test analysis). 
#' 
#' @param root Folder where to recursively look for the expanded tests. The tests must be located in files with extension either r or R. All other files are ignored.
#' @param verbose If TRUE, each test will be reported to the stdout as soon as it was executed and analyzed.
#' @param summary If TRUE, at the end of each analyzed file, a summary of all its tests will be printed. The summary contains the name and id of the test, its result and possibly comments to the reason for its failure.
#' @param displayOnlyErrors If TRUE, in either verbose mode, or summary, detailed information will only be printed about failed tests.
#' @param stopOnError If TRUE, first failed test will also terminate the execution of the test suite.
#' @param displayCodeOnError if TRUE, the code of the test will be displayed when the test fails (only relevant for verbose mode). To determine the code, deparse function is used.
#' @param testListener A function that will be called for each test. If supplied, it must be a proper test listener function whuich is checked by the is.testListener function (a proper test listener function has exactly five arguments, id, name, result, filename and comments).
#'  
#' @return TRUE if all tests have passed, FALSE otherwise. 
#' 
#' @seealso test, is.testListener
#' 
#' @examples runTests("c:/tests", verbose = TRUE)
#' 
#' f <- function(id, name, result, filename, comments) {
#'   if (result == "FAIL")
#'       cat("Test",name,"failed.\n")
#' }
#' runTests("c:/tests", testListener = f)

runTests <- function(root, verbose = FALSE, summary = FALSE, displayOnlyErrors = FALSE, stopOnError = FALSE, displayCodeOnError = TRUE, testListener = NULL) {
    if (!missing(testListener)) 
        if (! is.testListener(testListener))
            stop("Invalid function supported as a test listener.")
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
        tests <<- list(c("Test Name","Result", "Comments", "Id"))
        fails <<- 0
        passes <<- 0
        source(filename, local = FALSE)
        # invoke the test listener so that the results can be grabbed
        if (!is.null(testListener))
            for (t in tests[-1])
                testListener(t[[4]], t[[1]], t[[2]], filename, t[[3]])
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
    cat(sprintf("%-80s", test[[1]]),if (test[[2]]) "PASS" else "FAIL","\n")
    if (test[[2]] == FALSE) {
        cat(" ",test[[3]], "\n")
        if (displayCodeOnError && ! missing(code)) {
            cat("  Code:\n")
            for (l in deparse(code))
                cat("    ", l, "\n", sep="")
        }
    }
}

#' Comparing the results, also only to be used internally
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
#' The test is a success if the expected output is identical to the actual output and expected (or none) warnings have been reported during the execution, or if the code itself failed and the expected error has been found. Two NA values are always identical regardless their type. 
#' 
#' This function effectively defines the test and should be used in the test files. However, the test should only be executed by the runTests function which also prepares the necessary environment for the test function. 
#' 
#' @param id the unique id of the test in the runTests. 
#' @param code The code of the test, must be a runnable R code.
#' @param o Output of the test, if not specified no output will be checked (in case of an error expected)
#' @param w String to find in the warning messages (scalar or vector)
#' @param e String to find in the error messages (scalar or vector)
#' @param name Name of the test. If no name is specified, the index of the test in the file will be used.
#' 
#' @returns TRUE if the test passes, FALSE otherwise
#'
#' @seealso runTests
#' 
#' @examples 
#' test(id = 1, 
#'   {
#'     a = 1
#'     b = 2
#'     a + b
#'   }, 2, name = "simple test")
#' 
#' test(id = 2, 
#'   {
#'     warning("example warning")
#'     TRUE
#'   }, TRUE, expectWarning = "warning", name = "warning example)

#' test(id = 3,
#'   {
#'     stop("error")
#'     TRUE
#'   }, expectError = "error", name = "error example)
#' 
test <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
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
    if (!is.null(errors)) {
        result <- TRUE
    } else if (compareResults(result, o)) {
        result <- TRUE
    } else {
        appendComment("Expected",o, "got", result)
        result <- FALSE
    }
    # check the warnings
    if (missing(w)) {
        if (!is.null(w)) {
            result <- FALSE
            appendComment("Expected no warnings, but", warnings,"found")
        }
    } else {
        for (ww in w) 
            if (length(grep(ww, warnings)) == 0) {
                result <- FALSE
                appendComment("Warning", ww, "not found in", warnings)
            }
    }
    # check the errors
    if (missing(e)) {
        if (!is.null(errors)) {
            result <- FALSE
            appendComment("Expected no errors, but", errors,"found")
        }
    } else {
        for (ee in e) 
            if (length(grep(ee, errors)) == 0) {
                result <- FALSE
                appendComment("Error", ee, "not found in ", errors)
            }
    }
    if (missing(name))
        name <- as.character(length(tests))
    tests[[length(tests) + 1]] <<- c(paste("[",id,"] ",name, sep = ""), result, comments, id)
    if (verbose)
        print.test(tests[[length(tests)]], code)
    if (result) {
        passes <<- passes + 1
        TRUE
    } else {
        fails <<- fails + 1
        if (stopOnError) {
            stop("Test id ", id, " name " ,name," failed: ", comments)
        }
        FALSE
    }
}
