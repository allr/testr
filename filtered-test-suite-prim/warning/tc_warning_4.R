expected <- eval(parse(text="\"‘graphics’ namespace cannot be unloaded:\\n  namespace ‘graphics’ is imported by ‘stats’ so cannot be unloaded\""));      
test(id=0, code={      
argv <- eval(parse(text="list(FALSE, FALSE, \"‘graphics’ namespace cannot be unloaded:\\n  namespace ‘graphics’ is imported by ‘stats’ so cannot be unloaded\")"));      
.Internal(warning(argv[[1]], argv[[2]], argv[[3]]));      
      
}, o=expected);      
      
