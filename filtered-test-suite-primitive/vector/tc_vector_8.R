expected <- eval(parse(text="raw(0)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"raw\", 0L)"));           
.Internal(vector(argv[[1]], argv[[2]]));           
           
}, o=expected);           
           
