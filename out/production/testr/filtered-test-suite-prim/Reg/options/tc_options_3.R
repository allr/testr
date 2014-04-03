expected <- eval(parse(text="structure(list(width = 80L), .Names = \"width\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"width\")"));   
.Internal(`options`(argv[[1]]));   
   
}, o=expected);   
   
