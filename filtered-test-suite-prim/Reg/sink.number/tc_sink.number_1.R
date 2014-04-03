expected <- eval(parse(text="1L"));  
test(id=0, code={  
argv <- eval(parse(text="list(TRUE)"));  
.Internal(`sink.number`(argv[[1]]));  
  
}, o=expected);  
  
