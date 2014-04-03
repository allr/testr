expected <- eval(parse(text="c(FALSE, FALSE)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"./myTst2/Meta/package.rds\", \"./myTst2/INDEX\"))"));     
.Internal(`file.exists`(argv[[1]]));     
     
}, o=expected);     
     
