expected <- eval(parse(text="\"/home/roman/r-instrumented/library/tools\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library/tools\", \"/\", TRUE)"));  
.Internal(`normalizePath`(argv[[1]], argv[[2]], argv[[3]]));  
  
}, o=expected);  
  
