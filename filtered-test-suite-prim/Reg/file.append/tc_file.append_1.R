expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/tmp/RtmptPgrXI/file55711ba85492.R\", \"/tmp/RtmptPgrXI/file55711ba85492\")"));  
.Internal(`file.append`(argv[[1]], argv[[2]]));  
  
}, o=expected);  
  
