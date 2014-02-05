expected <- eval(parse(text="structure(list(), .Names = character(0), row.names = integer(0), .S3Class = \"data.frame\", extra = character(0))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), .S3Class = \"data.frame\", extra = character(0)), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
