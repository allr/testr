test({
    warning("This is a test of warning")
    TRUE
}, TRUE, expectWarning = "test of warning")

test({
    warning("This is a test of unexpected warning")
    TRUE
}, TRUE)
