expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- list();            
do.call(`all`, argv);            
}, o=expected);            
            
