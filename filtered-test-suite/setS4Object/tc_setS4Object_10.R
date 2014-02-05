expected <- eval(parse(text="structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"object\", package = \"methods\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"object\", package = \"methods\"), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
