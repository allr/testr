library(testr)
library(testthat)

context("Decoration")

test_that('Can decorate functions', {
    testr_options("verbose", FALSE)
    functions <- builtins()[sample(length(builtins()), 25)]
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
})
