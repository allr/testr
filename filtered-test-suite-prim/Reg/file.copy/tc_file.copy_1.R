expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/tmp/RtmptPgrXI/file55711ba85492.R\", \"./myTst/R\", FALSE, FALSE, TRUE)"));  
.Internal(`file.copy`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));  
  
}, o=expected);  
  
