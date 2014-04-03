expected <- eval(parse(text="\"there is no package called 'df0'\""));      
test(id=0, code={      
argv <- eval(parse(text="list(TRUE, FALSE, \"there is no package called 'df0'\")"));      
.Internal(warning(argv[[1]], argv[[2]], argv[[3]]));      
      
}, o=expected);      
      
