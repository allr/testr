expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"./myTst2\", 5)"));  
.Internal(`file.access`(argv[[1]], argv[[2]]));  
  
}, o=expected);  
  
