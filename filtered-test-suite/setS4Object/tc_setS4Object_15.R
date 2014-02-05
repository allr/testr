expected <- eval(parse(text="structure(1:10, .Tsp = c(1, 10, 1), .S3Class = \"ts\", class = structure(\"Ts\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(1:10, .Tsp = c(1, 10, 1), .S3Class = \"ts\", class = structure(\"Ts\", package = \".GlobalEnv\")), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
