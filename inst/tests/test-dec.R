library(testr)
library(testthat)

context("Decoration")

test_that('Can decorate functions', {
    testr_options("verbose", FALSE)
    builtin_capture()
    check.dec <- sapply(builtins(), function(x) {
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
    clear_decoration()
    check.dec <- sapply(builtins(), function(x) {
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
