expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"FALSE/src/library/translations/inst\", \"/home/lzhao/tmp/RtmptS6o2G/translations\", TRUE, TRUE, TRUE)"));    
.Internal(file.copy(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
    
}, o=expected);    
    
