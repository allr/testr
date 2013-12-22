expected <- eval(parse(text='3'));
test(id=0, code={
argv <- eval(parse(text='list(c(3, 0, 1), c(3, 0, 0), 0)'));
do.call("max", argv);
}, o=expected);

expected <- eval(parse(text='3'));
test(id=0, code={
  argv <- eval(parse(text='list(c(3, 0, 1), c(3, 0, 0), 4)'));
  do.call("max", argv);
}, o=expected);

expected <- eval(parse(text='1'));
test(id=0, code={
  argv <- eval(parse(text='list(c(0, 0, 1), c(0, 0, 0), 0)'));
  do.call("max", argv);
}, o=expected);

