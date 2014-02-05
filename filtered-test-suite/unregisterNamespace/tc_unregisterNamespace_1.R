expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(\"utils\", .Names = \"name\"))"));   
.Internal(unregisterNamespace(argv[[1]]));   
   
}, o=expected);   
   
