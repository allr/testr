expected <- eval(parse(text="\"'f' is deprecated.\\nUse 'convertUnit' instead.\\nSee help(\\\"Deprecated\\\")\""));      
test(id=0, code={      
argv <- eval(parse(text="list(FALSE, FALSE, \"'f' is deprecated.\\nUse 'convertUnit' instead.\\nSee help(\\\"Deprecated\\\")\")"));      
.Internal(warning(argv[[1]], argv[[2]], argv[[3]]));      
      
}, o=expected);      
      
