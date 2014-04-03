expected <- eval(parse(text="TRUE")); 
test(id=0, code={ 
argv <- eval(parse(text="list(\"./myTst/R/file55711ba85492\", \"./myTst/R/zfile55711ba85492.R\")")); 
.Internal(`file.rename`(argv[[1]], argv[[2]])); 
 
}, o=expected); 
 
