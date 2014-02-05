expected <- eval(parse(text="\"LC_CTYPE=C;LC_NUMERIC=C;LC_TIME=C;LC_COLLATE=C;LC_MONETARY=C;LC_MESSAGES=en_US.UTF-8;LC_PAPER=C;LC_NAME=C;LC_ADDRESS=C;LC_TELEPHONE=C;LC_MEASUREMENT=en_US.UTF-8;LC_IDENTIFICATION=C\""));    
test(id=0, code={    
argv <- eval(parse(text="list(1L, \"C\")"));    
.Internal(Sys.setlocale(argv[[1]], argv[[2]]));    
    
}, o=expected);    
    
