expected <- eval(parse(text="structure(list(a = c(1.65188605106614, 3.36741098258348, -5.82097822825024, 3.75017123675711, 1.30212114063423, -1.59507405122883, 2.2566316783288, 1.40479822473896, 1.68879990772286, 2.59335487411388), b = structure(c(1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L), .Label = c(\"F\", \"T\"), class = \"factor\"), c = 1:10), .Names = c(\"a\", \"b\", \"c\"), row.names = c(\"R:1\", \"R:2\", \"R:3\", \"R:4\", \"R:5\", \"R:6\", \"R:7\", \"R:8\", \"R:9\", \"R:10\"), .S3Class = \"data.frame\", extra = \"testing\", class = structure(\"myData\", package = \".GlobalEnv\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(list(a = c(1.65188605106614, 3.36741098258348, -5.82097822825024, 3.75017123675711, 1.30212114063423, -1.59507405122883, 2.2566316783288, 1.40479822473896, 1.68879990772286, 2.59335487411388), b = structure(c(1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L), .Label = c(\"F\", \"T\"), class = \"factor\"), c = 1:10), .Names = c(\"a\", \"b\", \"c\"), row.names = c(\"R:1\", \"R:2\", \"R:3\", \"R:4\", \"R:5\", \"R:6\", \"R:7\", \"R:8\", \"R:9\", \"R:10\"), .S3Class = \"data.frame\", extra = \"testing\", class = structure(\"myData\", package = \".GlobalEnv\")), TRUE, 0L)"));           
.Internal(setS4Object(argv[[1]], argv[[2]], argv[[3]]));           
           
}, o=expected);           
           
