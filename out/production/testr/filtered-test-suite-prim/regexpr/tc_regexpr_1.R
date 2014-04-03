expected <- eval(parse(text="structure(13L, match.length = 2L)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"EN\", \"«Latin-1 accented chars»: éè øØ å<Å æ<Æ é éè\", TRUE, TRUE, FALSE, FALSE)"));           
.Internal(regexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
           
}, o=expected);           
           
