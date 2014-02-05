expected <- eval(parse(text="NULL"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"/home/lzhao/tmp/RtmpmjWNym/Rtxt6f4c31657a7c\", \"\", \"R Help on ‘USArrests’\", TRUE, \"/home/lzhao/hg/r-instrumented/bin/pager\")"));    
.Internal(file.show(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
    
}, o=expected);    
    
