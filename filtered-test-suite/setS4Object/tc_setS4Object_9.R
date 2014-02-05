expected <- eval(parse(text="structure(1, .Dim = c(1L, 1L), a = c(NA, 3, -1, 2), class = structure(\"B\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(1, .Dim = c(1L, 1L), a = c(NA, 3, -1, 2), class = structure(\"B\", package = \".GlobalEnv\")), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
