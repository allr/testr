expected <- eval(parse(text="\"/home/roman/r-instrumented/library/lattice\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library\", \"^lattice$\", FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)"));  
.Internal(`list.files`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));  
  
}, o=expected);  
  
