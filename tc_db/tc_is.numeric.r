expected <- eval(parse(text='TRUE'));
test(id=0, code={
argv <- eval(parse(text='list(0.1)'));
do.call("is.numeric", argv);
}, o=expected);

