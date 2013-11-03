expected <- eval(parse(text='c(1, 1, 1)'));
test(id=0, code={
argv <- eval(parse(text='list(structure(c(1, 1, 1), .Names = c("insertions", "deletions", "substitutions")))'));
do.call("as.integer", argv);
}, o=expected);

