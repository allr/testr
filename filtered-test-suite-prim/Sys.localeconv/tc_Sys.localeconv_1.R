expected <- eval(parse(text="structure(c(\".\", \"\", \"\", \"USD \", \"$\", \".\", \",\", \"\\003\\003\", \"\", \"-\", \"2\", \"2\", \"1\", \"0\", \"1\", \"0\", \"1\", \"1\"), .Names = c(\"decimal_point\", \"thousands_sep\", \"grouping\", \"int_curr_symbol\", \"currency_symbol\", \"mon_decimal_point\", \"mon_thousands_sep\", \"mon_grouping\", \"positive_sign\", \"negative_sign\", \"int_frac_digits\", \"frac_digits\", \"p_cs_precedes\", \"p_sep_by_space\", \"n_cs_precedes\", \"n_sep_by_space\", \"p_sign_posn\", \"n_sign_posn\"))"));  
test(id=0, code={  
.Internal(`Sys.localeconv`());  
}, o=expected);  
  
