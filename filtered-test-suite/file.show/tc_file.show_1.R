expected <- eval(parse(text="NULL"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"/home/lzhao/tmp/RtmptS6o2G/RlibraryIQR731f158a2c10\", \"\", \"R packages available\", TRUE, \"cat\")"));    
.Internal(file.show(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
    
}, o=expected);    
    
