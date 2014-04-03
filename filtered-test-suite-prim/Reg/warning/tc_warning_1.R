expected <- eval(parse(text="\"1 y value <= 0 omitted from logarithmic plot\""));  
test(id=0, code={  
argv <- eval(parse(text="list(TRUE, FALSE, \"1 y value <= 0 omitted from logarithmic plot\")"));  
.Internal(`warning`(argv[[1]], argv[[2]], argv[[3]]));  
  
}, o=expected);  
  
