expected <- eval(parse(text="structure(NA, .Tsp = c(1, 1, 1), class = structure(\"ts\", package = \"methods\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(NA, .Tsp = c(1, 1, 1), class = structure(\"ts\", package = \"methods\")), FALSE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
