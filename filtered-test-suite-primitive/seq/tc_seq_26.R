expected <- eval(parse(text="1:4"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(Topic = character(0), File = character(0), Title = character(0), Internal = character(0)), .Names = c(\"Topic\", \"File\", \"Title\", \"Internal\"), row.names = integer(0), class = \"data.frame\"))"));               
do.call(`seq_along`, argv);               
}, o=expected);               
               
