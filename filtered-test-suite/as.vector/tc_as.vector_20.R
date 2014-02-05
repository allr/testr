expected <- eval(parse(text="NA_integer_"));               
test(id=0, code={               
argv <- eval(parse(text="list(NA, \"integer\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
               
}, o=expected);               
               
