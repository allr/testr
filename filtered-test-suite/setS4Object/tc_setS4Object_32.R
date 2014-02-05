expected <- eval(parse(text="structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4.17, 5.58, 5.18, 6.11, 4.5, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69), .Dim = c(20L, 2L), date = structure(16045, class = \"Date\"), class = structure(\"MatX\", package = \".GlobalEnv\"), .Dimnames = list(NULL, c(\"group\", \"weight\")))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4.17, 5.58, 5.18, 6.11, 4.5, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69), .Dim = c(20L, 2L), date = structure(16045, class = \"Date\"), class = structure(\"MatX\", package = \".GlobalEnv\"), .Dimnames = list(NULL, c(\"group\", \"weight\"))), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
