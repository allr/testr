expected <- eval(parse(text="structure(list(size = 1056, isdir = FALSE, mode = structure(420L, class = \"octmode\"), mtime = 1393726881.70949, ctime = 1393726934.64149, atime = 1395074550.80596, uid = 0L, gid = 0L, uname = \"root\", grname = \"root\"), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library/Matrix/R/Matrix\")"));   
.Internal(`file.info`(argv[[1]]));   
   
}, o=expected);   
   
