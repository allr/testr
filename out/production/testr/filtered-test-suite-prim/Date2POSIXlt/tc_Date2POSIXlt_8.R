expected <- eval(parse(text="structure(list(sec = c(0, 0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L, 0L, 0L), mday = c(2L, 9L, 16L, 16L, 23L, 31L), mon = c(0L, 0L, 0L, 0L, 0L, 0L), year = c(60L, 60L, 60L, 60L, 60L, 60L), wday = c(6L, 6L, 6L, 6L, 6L, 0L), yday = c(1L, 8L, 15L, 15L, 22L, 30L), isdst = c(0L, 0L, 0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(-3652, -3644.75, -3637.5, -3637.5, -3630.25, -3623), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"), class = c(\"table\", \"Date\")))"));     
.Internal(Date2POSIXlt(argv[[1]]));     
     
}, o=expected);     
     
