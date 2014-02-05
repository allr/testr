expected <- eval(parse(text="c(0, 0, 0, 0, 0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(3.14159265358979, class = structure(\"3.14159265358979\", class = \"testit\")))"));  
do.call(`gc.time`, argv);  
}, o=expected);  
  
