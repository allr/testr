expected <- eval(parse(text="structure(list(size = c(NA, NA, NA, NA, 4096, NA, NA, NA), isdir = c(NA, NA, NA, NA, TRUE, NA, NA, NA), mode = structure(c(NA, NA, NA, NA, 493L, NA, NA, NA), class = \"octmode\"), mtime = c(NA, NA, NA, NA, 1395082004.94188, NA, NA, NA), ctime = c(NA, NA, NA, NA, 1395082004.94188, NA, NA, NA), atime = c(NA, NA, NA, NA, 1395082010.93388, NA, NA, NA), uid = c(NA, NA, NA, NA, 1001L, NA, NA, NA), gid = c(NA, NA, NA, NA, 1001L, NA, NA, NA), uname = c(NA, NA, NA, NA, \"roman\", NA, NA, NA), grname = c(NA, NA, NA, NA, \"roman\", NA, NA, NA)), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(\"/home/roman/r-instrumented/library/stats/data\", \"/home/roman/r-instrumented/library/graphics/data\", \"/home/roman/r-instrumented/library/grDevices/data\", \"/home/roman/r-instrumented/library/utils/data\", \"/home/roman/r-instrumented/library/datasets/data\", \"/home/roman/r-instrumented/library/methods/data\", \"/home/roman/r-instrumented/library/base/data\", \"/home/roman/r-instrumented/tests/data\"))"));   
.Internal(`file.info`(argv[[1]]));   
   
}, o=expected);   
   
