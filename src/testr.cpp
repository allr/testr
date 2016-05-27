#include "testr.h"
#define USE_RINTERNALS
using namespace Rcpp;
using namespace std;

SEXP search(){
    return Rf_eval(Language(".Internal", Language("search")), R_GlobalEnv);
}

SEXP deparse(SEXP x)
{
    Language call("deparse", x, Rf_ScalarInteger(60), Rf_ScalarLogical(1), Rf_ScalarReal(69), Rf_ScalarInteger(-1));
    Language intCall(".Internal", call);
    return Rf_eval(intCall, R_GlobalEnv);
}


bool missing(SEXP x, SEXP env)
{
    // missing is blacklisted
    LogicalVector res(Rf_eval(Language("missing", x), env));
    return res[0] == TRUE;
}

bool contains(CharacterVector v, string elem){
  for (int i = 0; i < v.length(); i++){
    if (elem == as<string>(v[i]))
    return true;
  }
  return false;
}

string getFunctionEnvironmentName(string &functionName) {
  string envir_name = "";
  Environment envir;
  CharacterVector packages = search();
  for (int i = 0; i < packages.length(); i++){
    envir = Environment(as<std::string>(packages[i]));
    if (envir.exists(functionName)) {
      envir_name = as<std::string>(packages[i]);
      break;
    }
  }
  return envir_name;
}
