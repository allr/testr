expected <- eval(parse(text="NULL"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"/home/lzhao/tmp/RtmptS6o2G/RpackageInfo731fded925a\", \"\", character(0), TRUE, \"cat\")"));    
.Internal(file.show(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
    
}, o=expected);    
    
