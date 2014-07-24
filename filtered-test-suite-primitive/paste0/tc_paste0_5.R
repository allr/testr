expected <- eval(parse(text="character(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(character(0), character(0), character(0)), NULL)"));       
.Internal(`paste0`(argv[[1]], argv[[2]]));       
       
}, o=expected);       
       
