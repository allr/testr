expected <- eval(parse(text="structure(c(\"MASS\", \"7.3-26\"), .Names = c(\"Package\", \"Version\"))"));  
test(id=0, code={  
FUN<-  function (p)    
  {   
      pfile <- file.path(p, "Meta", "package.rds")   
      info <- if (file.exists(pfile))    
          readRDS(pfile)$DESCRIPTION[c("Package", "Version")]   
      else {   
          info <- tryCatch(read.dcf(file.path(p, "DESCRIPTION"), c("Package", "Version"))[1, ], error = identity)   
          if (inherits(info, "error") || (length(info) != 2L) || any(is.na(info)))    
              c(Package = NA, Version = NA)   
          else info   
      }   
  }   
  
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library/MASS\")"));  
do.call(`FUN`, argv);  
}, o=expected);  
  
