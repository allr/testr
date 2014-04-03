expected <- eval(parse(text="\"/home/roman/r-instrumented/library/base/help/Cstack_info\""));     
test(id=0, code={     
argv <- eval(parse(text="list(list(\"/home/roman/r-instrumented/library/base\", \"help\", structure(\"Cstack_info\", .Names = \"Cstack_info\")), \"/\")"));     
.Internal(`file.path`(argv[[1]], argv[[2]]));     
     
}, o=expected);     
     
