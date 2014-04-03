expected <- eval(parse(text="list(structure(c(27L, 43L), match.length = c(2L, 2L)))"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"éè\", \"«Latin-1 accented chars»: éè øØ å<Å æ<Æ é éè\", FALSE, FALSE, TRUE, FALSE)"));      
.Internal(gregexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));      
      
}, o=expected);      
      
