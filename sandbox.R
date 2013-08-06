# testr sandbox




testSuite <- function(root, verbose = FALSE, summary = FALSE, displayOnlyErrors = FALSE, stopOnError = FALSE, displayCodeOnError = TRUE) {
    verbose <<- verbose
    displayOnlyErrors <<- displayOnlyErrors
    stopOnError <<- stopOnError
    displayCodeOnError <<- displayCodeOnError
    totalFail <- 0
    totalPass <- 0
    if (verbose)
        cat(sprintf("%-80s", "Name"),"Result\n---------------------------------------------------------------------------------------\n")
    for (f in list.files(root, pattern=".[rR]$", recursive = TRUE)) {
        filename <- paste(root,"/", f, sep = "")
        cat(filename,"...\n")
        tests <<- list(c("Test Name","Result", "Comments"))
        fail <<- 0
        pass <<- 0
        source(filename, local = FALSE)
        cat("  (pass = ", pass,", fail = ", fail, ", total = ", pass + fail, ")\n", sep = "")
        totalFail <- totalFail + fail
        totalPass <- totalPass + pass
        if (summary == TRUE) {
            cat(sprintf("%-80s", "Name"),"Result\n---------------------------------------------------------------------------------------\n")
            for (t in tests[-1])
                print.test(t)
            cat("\n")
        }
    }
    rm(tests, envir = globalenv())
    rm(fail, envir = globalenv())
    rm(pass, envir = globalenv())
    rm(verbose, envir = globalenv())
    rm(displayOnlyErrors, envir = globalenv())
    rm(stopOnError, envir = globalenv())
    rm(displayCodeOnError, envir = globalenv())
    cat("\n-------- Results --------\n")
    cat("Passed:   ", totalPass, "\n")
    cat("Failed:   ", totalFail, "\n")
    cat("Total:    ", totalPass + totalFail, "\n")
    if (totalFail == 0)
        cat("Overall:  ", "PASS\n")
    else
        cat("Overall:  ", "FAIL\n")
} 

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
    if (identical(result, output)) {
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
        pass <<- pass + 1
    } else {
        fail <<- fail + 1
        if (stopOnError) {
            stop("Test ",name," failed: ", comments)
        }
    }
}

testSuite("c:/delete/tests")
