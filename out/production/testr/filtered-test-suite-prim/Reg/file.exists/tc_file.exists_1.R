expected <- eval(parse(text="FALSE"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"./myTst/R//filelist\")"));     
.Internal(`file.exists`(argv[[1]]));     
     
}, o=expected);     
     
