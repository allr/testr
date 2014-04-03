expected <- eval(parse(text="c(0L, 0L, 1L, 0L, 22L, 9L, 2L, 6L, 6L, 0L, 0L, 0L, 0L, 0L, 0L, 9L, 2L, 5L, 7L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"\", \"\", \"\\036\", \"\", \"anyDuplicated(<array>,\", \"MARGIN=0)\", \"no\", \"longer\", \"fails.\", \"\", \"\", \"\", \"\", \"\", \"\", \"(Reported\", \"by\", \"Hervé\", \"Pagès.)\"), \"w\", FALSE)"));                
.Internal(nchar(argv[[1]], argv[[2]], argv[[3]]));                
                
}, o=expected);                
                
