expected <- eval(parse(text="c(\"/home/roman/r-instrumented/library/stats/data\", \"/home/roman/r-instrumented/library/graphics/data\", \"/home/roman/r-instrumented/library/grDevices/data\", \"/home/roman/r-instrumented/library/utils/data\", \"/home/roman/r-instrumented/library/datasets/data\", \"/home/roman/r-instrumented/library/methods/data\", \"/home/roman/r-instrumented/library/base/data\", \"/home/roman/r-instrumented/tests/data\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(list(c(\"/home/roman/r-instrumented/library/stats\", \"/home/roman/r-instrumented/library/graphics\", \"/home/roman/r-instrumented/library/grDevices\", \"/home/roman/r-instrumented/library/utils\", \"/home/roman/r-instrumented/library/datasets\", \"/home/roman/r-instrumented/library/methods\", \"/home/roman/r-instrumented/library/base\", \"/home/roman/r-instrumented/tests\"), \"data\"), \"/\")"));     
.Internal(`file.path`(argv[[1]], argv[[2]]));     
     
}, o=expected);     
     
