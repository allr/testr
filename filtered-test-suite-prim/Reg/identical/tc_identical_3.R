expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(function (e1, e2) standardGeneric(\"Logic\"), generic = structure(\"Logic\", package = \"base\"), package = \"base\", group = list(\"Ops\"), valueClass = character(0), signature = c(\"e1\", \"e2\"), default = quote(`\\001NULL\\001`), skeleton = quote((function (e1, e2) stop(\"invalid call in method dispatch to 'Logic' (no default method)\", domain = NA))(e1, e2)), groupMembers = list(\"&\", \"|\"), class = structure(\"groupGenericFunction\", package = \"methods\")), structure(function (e1, e2) standardGeneric(\"Logic\"), generic = structure(\"Logic\", package = \"base\"), package = \"base\", group = list(\"Ops\"), valueClass = character(0), signature = c(\"e1\", \"e2\"), default = quote(`\\001NULL\\001`), skeleton = quote((function (e1, e2) stop(\"invalid call in method dispatch to 'Logic' (no default method)\", domain = NA))(e1, e2)), groupMembers = list(\"&\", \"|\"), class = structure(\"groupGenericFunction\", package = \"methods\")), TRUE, TRUE, TRUE, TRUE, FALSE)"));      
.Internal(`identical`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));      
      
}, o=expected);      
      
