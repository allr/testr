library(testr)
library(testthat)

context("Decoration")

test_decoration <- function(functions) {
    testr_options("verbose", F)
    suppressWarnings(setup_capture(functions))
    check.dec <- sapply(functions, function(x) {
        if (!exists(x, envir = getNamespace('base')))
            return(TRUE)
        obj <- get(x, envir = getNamespace('base'))
        if (is.function(obj)) {
            class(obj) == "functionWithTrace" || !testr:::eligible_capture(x) || testr:::is_s3_generic(x)
        } else {
            TRUE
        }
    })
    expect_true(length(testr:::.decorated) > 0)
    expect_true(all(check.dec))
    stop_capture_all()
    check.dec <- sapply(functions, function(x) {
        if (!exists(x, envir = getNamespace('base')))
            return(FALSE)
        obj <- get(x, envir = getNamespace('base'))
        if (is.function(obj))
            class(obj) == "functionWithTrace" && x != "library"
        else
            FALSE
    })
    expect_true(length(testr:::.decorated) == 0)
    expect_false(any(check.dec))
}



test_that('Can decorate functions (long)', {
    skip_on_cran()
    # TODO reenable!!
    #functions <- builtins()
    #test_decoration(functions)
})


test_that('Can decorate functions', {
    # to get rid of randomness
    functions <- c("any", "alist", "double", "deparse", "is.logical", "isOpen", "log2", "mean.Date", "pmax", "sort", "sweep", "unsplit")
    test_decoration(functions)
})
