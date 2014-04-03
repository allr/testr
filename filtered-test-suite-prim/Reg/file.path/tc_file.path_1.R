expected <- eval(parse(text="\"/home/roman/r-instrumented/tests/myLib/pkgA/Meta/package.rds\""));     
test(id=0, code={     
argv <- eval(parse(text="list(list(\"/home/roman/r-instrumented/tests/myLib/pkgA\", \"Meta\", \"package.rds\"), \"/\")"));     
.Internal(`file.path`(argv[[1]], argv[[2]]));     
     
}, o=expected);     
     
