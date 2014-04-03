expected <- eval(parse(text="c(\"list(\\\"Härdle\\\")\", \"list(\\\"Haerdle\\\")\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(list(structure(\"Härdle\", Rd_tag = \"TEXT\")), list(structure(\"Haerdle\", Rd_tag = \"TEXT\"))), Rd_tag = \"\\\\enc\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 
                 
