expected <- eval(parse(text="structure(1386500049.77571, class = c(\"POSIXct\", \"POSIXt\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(1386500049.77571, class = c(\"POSIXct\", \"POSIXt\")), TRUE, TRUE)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
