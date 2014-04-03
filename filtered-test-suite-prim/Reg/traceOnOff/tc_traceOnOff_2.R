expected <- eval(parse(text="TRUE")); 
test(id=0, code={ 
argv <- eval(parse(text="list(FALSE)")); 
.Internal(`traceOnOff`(argv[[1]])); 
 
}, o=expected); 
 
