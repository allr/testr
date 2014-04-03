expected <- eval(parse(text="134L"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"text> ## Two more latin1 examples\\ntext> text(5, 10.2,\\ntext+      \\\"Le français, c'est façile: Règles, Liberté, Egalité, Fraternité...\\\")\", \"c\", FALSE)"));     
.Internal(`nchar`(argv[[1]], argv[[2]], argv[[3]]));     
     
}, o=expected);     
     
