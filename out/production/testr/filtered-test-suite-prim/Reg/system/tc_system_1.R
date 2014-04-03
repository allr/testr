expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"'/home/roman/r-instrumented/bin/R' CMD INSTALL -l '/home/roman/r-instrumented/tests/myLib' 'myTst'\", FALSE)"));  
.Internal(`system`(argv[[1]], argv[[2]]));  
  
}, o=expected);  
  
