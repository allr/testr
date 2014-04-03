expected <- eval(parse(text="\"text> ## The following two examples use latin1 characters: these may not\\ntext> ## appear correctly (or be omitted entirely).\\ntext> plot(1:10, 1:10, main = \\\"text(...) examples\\\\n~~~~~~~~~~~~~~\\\",\\ntext+      sub = \\\"R is GNU ©, but not ® ...\\\")\""));    
test(id=0, code={    
argv <- eval(parse(text="list(\"text> ## The following two examples use latin1 characters: these may not\\ntext> ## appear correctly (or be omitted entirely).\\ntext> plot(1:10, 1:10, main = \\\"text(...) examples\\\\n~~~~~~~~~~~~~~\\\",\\ntext+      sub = \\\"R is GNU ©, but not ® ...\\\")\", 1L, 238L)"));    
.Internal(`substr`(argv[[1]], argv[[2]], argv[[3]]));    
    
}, o=expected);    
    
