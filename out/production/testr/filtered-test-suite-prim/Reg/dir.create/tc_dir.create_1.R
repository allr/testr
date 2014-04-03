expected <- eval(parse(text="TRUE")); 
test(id=0, code={ 
argv <- eval(parse(text="list(\"/tmp/RtmptPgrXI/Pkgs/pkgB/R\", FALSE, TRUE, structure(511L, class = \"octmode\"))")); 
.Internal(`dir.create`(argv[[1]], argv[[2]], argv[[3]], argv[[4]])); 
 
}, o=expected); 
 
