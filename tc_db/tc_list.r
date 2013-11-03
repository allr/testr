expected <- eval(parse(text='list(structure(list(major = "3", minor = "0.1"), .Names = c("major", "minor"), class = "simple.list"))'));
test(id=0, code={
argv <- eval(parse(text='list(structure(list(major = "3", minor = "0.1"), .Names = c("major", "minor"), class = "simple.list"))'));
do.call("list", argv);
}, o=expected);

