expected <- eval(parse(text="NULL"));  
test(id=0, code={  
writeLines<-  function (text, con = stdout(), sep = "\n", useBytes = FALSE)    
  {   
      if (is.character(con)) {   
          con <- file(con, "w")   
          on.exit(close(con))   
      }   
      .Internal(writeLines(text, con, sep, useBytes))   
  }   
  
argv <- eval(parse(text="list(c(\"setClass(\\\"foo\\\", contains=\\\"numeric\\\")\", \"setMethod(\\\"show\\\", \\\"foo\\\",\", \"          function(object) cat(\\\"I am a \\\\\\\"foo\\\\\\\"\\\\n\\\"))\"), \"/tmp/RtmptPgrXI/file55711ba85492\")"));  
do.call(`writeLines`, argv);  
}, o=expected);  
  
