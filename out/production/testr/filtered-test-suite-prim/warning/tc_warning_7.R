expected <- eval(parse(text="\" Section NULL is unrecognized and will be dropped\""));      
test(id=0, code={      
argv <- eval(parse(text="list(FALSE, TRUE, \" Section NULL is unrecognized and will be dropped\")"));      
.Internal(warning(argv[[1]], argv[[2]], argv[[3]]));      
      
}, o=expected);      
      
