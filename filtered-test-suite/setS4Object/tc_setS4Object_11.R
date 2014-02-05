expected <- eval(parse(text="structure(NA, .Dim = c(1L, 1L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(NA, .Dim = c(1L, 1L)), TRUE, TRUE)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
