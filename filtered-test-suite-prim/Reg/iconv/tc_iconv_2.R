expected <- eval(parse(text="\"•\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"•\", \"UTF-8\", \"\", NA_character_, TRUE, FALSE)"));  
.Internal(`iconv`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));  
  
}, o=expected);  
  
