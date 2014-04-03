expected <- eval(parse(text="structure(7L, match.length = 1L)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\" \", \"façile a \", FALSE, FALSE, FALSE, FALSE)"));           
.Internal(regexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
           
}, o=expected);           
           
