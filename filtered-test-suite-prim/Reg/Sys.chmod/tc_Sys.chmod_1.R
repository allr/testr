expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/tmp/RtmptPgrXI/file55711ba85492.R\", structure(436L, class = \"octmode\"), TRUE)"));  
.Internal(`Sys.chmod`(argv[[1]], argv[[2]], argv[[3]]));  
  
}, o=expected);  
  
