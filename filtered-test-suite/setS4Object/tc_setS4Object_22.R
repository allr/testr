expected <- eval(parse(text="structure(1:10, date = structure(200171400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), class = structure(\"stamped\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(1:10, date = structure(200171400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), class = structure(\"stamped\", package = \".GlobalEnv\")), FALSE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
