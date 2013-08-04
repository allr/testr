test(name = "NA vs NaN", 
    c(NA + NaN, NaN + NA),
    c(NA, NA)
)

test(name = "simple test 1",
    1,
    1
)

test(2, 2)

test(name = "a failure",
    1, 
    2
)

