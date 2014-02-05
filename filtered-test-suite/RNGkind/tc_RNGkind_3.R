expected <- eval(parse(text="3:4"));    
test(id=0, code={    
argv <- eval(parse(text="list(1L, 0L)"));    
.Internal(RNGkind(argv[[1]], argv[[2]]));    
    
}, o=expected);    
    
