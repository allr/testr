expected <- eval(parse(text="\"text> text(5, 9.8,\\ntext+      \\\"Jetz no chli züritüütsch: (noch ein bißchen Zürcher deutsch)\\\")\""));                
test(id=0, code={                
argv <- eval(parse(text="list(\"text> text(5, 9.8,\\ntext+      \\\"Jetz no chli züritüütsch: (noch ein bißchen Zürcher deutsch)\\\")\", 1L, 93L)"));                
.Internal(substr(argv[[1]], argv[[2]], argv[[3]]));                
                
}, o=expected);                
                
