expected <- eval(parse(text="\"C\""));    
test(id=0, code={    
argv <- eval(parse(text="list(8L)"));    
.Internal(Sys.getlocale(argv[[1]]));    
    
}, o=expected);    
    
