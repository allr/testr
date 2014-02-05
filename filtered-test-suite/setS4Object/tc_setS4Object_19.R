expected <- eval(parse(text="structure(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), id = character(0), class = structure(\"withId\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), id = character(0), class = structure(\"withId\", package = \".GlobalEnv\")), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
