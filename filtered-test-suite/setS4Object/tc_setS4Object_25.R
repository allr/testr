expected <- eval(parse(text="structure(3.14159265358979, comment = \"Start with pi\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(3.14159265358979, comment = \"Start with pi\"), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
