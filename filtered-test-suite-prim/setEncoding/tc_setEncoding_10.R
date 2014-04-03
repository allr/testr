expected <- eval(parse(text="\"façile\"   "));       
test(id=0, code={       
argv <- eval(parse(text="list(\"fa\\xe7ile\", \"latin1\")"));       
.Internal(setEncoding(argv[[1]], argv[[2]]));       
       
}, o=expected);       
       
