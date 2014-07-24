expected <- eval(parse(text="\"double\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(NA, 0.1945), .Names = c(\"1\", \"2\")))"));                
.Internal(typeof(argv[[1]]));                
                
}, o=expected);                
                
