expected <- eval(parse(text="c(1012633320, 32569542120)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 2L, hour = 2L, mday = 2L, mon = 1L, year = c(102L, 1102L), wday = 6L, yday = 32L, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")), \"\")"));      
.Internal(as.POSIXct(argv[[1]], argv[[2]]));      
      
}, o=expected);      
      
