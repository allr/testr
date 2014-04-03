expected <- eval(parse(text="TRUE")); 
test(id=0, code={ 
argv <- eval(parse(text="list(\"./myTst\", TRUE, FALSE, structure(511L, class = \"octmode\"))")); 
.Internal(`dir.create`(argv[[1]], argv[[2]], argv[[3]], argv[[4]])); 
 
}, o=expected); 
 
