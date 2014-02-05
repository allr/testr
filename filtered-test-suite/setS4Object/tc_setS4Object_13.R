expected <- eval(parse(text="structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L), class = structure(\"mmat2\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L), class = structure(\"mmat2\", package = \".GlobalEnv\")), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
