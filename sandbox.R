# testr sandbox


testSuite <- function(root) {
    for (f in list.files(root, pattern=".[rR]$", recursive = TRUE)) {
        filename <- paste(root,"/", f, sep = "")
        tests <<- list(c("Test Name","Result", "Comments"))
        source(filename, local = FALSE)
        cat(filename)
        cat("(pass = ", length(tests[ tests[[2]] == "PASS" ]), ", ")
        cat("fail = ", length(tests[ tests[[2]] == "FAIL"]), ", ")
        cat("total = ", length(tests) - 1, ")\n")
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
}

#testSuite("c:/delete/tests")