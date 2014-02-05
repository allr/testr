expected <- eval(parse(text="\"C\""));    
test(id=0, code={    
argv <- eval(parse(text="list(2L, \"C\")"));    
.Internal(Sys.setlocale(argv[[1]], argv[[2]]));    
    
}, o=expected);    
    
