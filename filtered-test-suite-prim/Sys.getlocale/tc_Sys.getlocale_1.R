expected <- eval(parse(text="\"en_US.UTF-8\""));    
test(id=0, code={    
argv <- eval(parse(text="list(3L)"));    
.Internal(Sys.getlocale(argv[[1]]));    
    
}, o=expected);    
    
