expected <- eval(parse(text="structure(numeric(0), .Dim = 0L)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(numeric(0), .Dim = 0L), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
