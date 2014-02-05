expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(\"/usr/bin/pdflatex\", .Names = \"pdflatex\"))"));            
.Internal(file.exists(argv[[1]]));            
            
}, o=expected);            
            
