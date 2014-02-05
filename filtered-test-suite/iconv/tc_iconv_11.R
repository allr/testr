expected <- eval(parse(text="NULL"));           
test(id=0, code={           
argv <- eval(parse(text="list(NULL, \"\", \"\", \"\", TRUE, FALSE)"));           
.Internal(iconv(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
           
}, o=expected);           
           
