expected <- eval(parse(text="structure(list(MBCS = TRUE, `UTF-8` = TRUE, `Latin-1` = FALSE), .Names = c(\"MBCS\", \"UTF-8\", \"Latin-1\"))"));    
test(id=0, code={    
.Internal(`l10n_info`());    
}, o=expected);    
    
