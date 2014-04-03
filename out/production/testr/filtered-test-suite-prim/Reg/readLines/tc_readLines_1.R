expected <- eval(parse(text="c(\"# line 1\", \"2+3\", \"ls()\", \"pi\", \"# last line\")"));  
test(id=0, code={  
readLines<-  function (con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown")    
  {   
      if (is.character(con)) {   
          con <- file(con, "r")   
          on.exit(close(con))   
      }   
      .Internal(readLines(con, n, ok, warn, encoding))   
  }   
  
argv <- eval(parse(text="list(\"testIO.R\")"));  
do.call(`readLines`, argv);  
}, o=expected);  
  
