expected <- eval(parse(text="\"glm.fit: algorithm did not converge\""));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE, \"glm.fit: algorithm did not converge\")"));  
.Internal(`warning`(argv[[1]], argv[[2]], argv[[3]]));  
  
}, o=expected);  
  
