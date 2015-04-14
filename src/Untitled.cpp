#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

std::string dput_c(SEXP x) {
  Function deparse("deparse");
  std::string result;
  if (x == R_NilValue) 
    return "NULL";
  if (TYPEOF(x) == LISTSXP || TYPEOF(x) == VECSXP) {
    List argsL(x);
    // problem is here
    List names = argsL.names();
    result += "list(";
    for (int i = 0; i < argsL.size(); i++) {
      if (names.size() > i) {
        std::string name = as<std::string>(names[i]);
        if (name != ""){
          result += name;
          result += "=";
        }
      }
      result += dput_c(argsL[i]);
      if (i != argsL.size() - 1) 
      result +=",\n";
    }
    result += ")";
  } else if (Rf_isS4(x)) {
    Rcpp::S4 obj(x);
    std::vector<std::string> snames= obj.attributeNames();
    result+= "new(\"" + as<std::string>(obj.attr("class")) + "\"";
    for (int i = 0; i < snames.size(); i++) {
      if (snames[i] != "class") {
        result += ",\n" + snames[i] + "=";
        result += dput_c(obj.slot(snames[i]));
      }
    }
    result+= ")";
  } else {
    if (TYPEOF(x) == LANGSXP) {
      result += "quote(";
      x = as<SEXP>(Language("quote", x));      
    }
    CharacterVector dx = deparse(x); 
    if (dx.length() < 1000) 
      for (int i = 0; i < dx.length(); i++) 
        result += as<std::string>(dx[i]);
    else 
        result += "<too long>";
    if (TYPEOF(x) == LANGSXP) 
      result += ")";
  }
  return result;
}