expected <- eval(parse(text="TRUE"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 2:3, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"))))"));     
do.call(`all`, argv);     
}, o=expected);     
     
