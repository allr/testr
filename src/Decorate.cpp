#include <Rcpp.h>
#include "testr.h"
using namespace std;
using namespace Rcpp;

map<string, SEXP> decorationChanges;

// [[Rcpp::export]]
bool DecorateSubst_cpp(CharacterVector packages, CharacterVector name, CharacterVector functionTypes) {
  Function DecorateBody("DecorateBody");  
  Function ReplaceBody("ReplaceBody"); 
  
  Environment testr("package:testr"); 
  SEXP obj;
  RObject robj;
  Environment envir_namespace;
  string envir_name;
  Environment envir;
  Function warning("warning");
  string functionName = as<std::string>(name[0]); 
  envir_name = getFunctionEnvironmentName(functionName);
  envir = Environment(envir_name);
  obj = envir.get(functionName);
  robj = RObject(obj);
  if (!robj.hasAttribute("decorated")){
    if (envir_name != ".GlobalEnv"){
      string namespace_name = envir_name.substr(8, string::npos);
      envir_namespace = Environment::namespace_env(namespace_name);
      envir_namespace.unlockBinding(functionName);
      envir.unlockBinding(functionName);
    } else {
      envir_namespace = Environment::global_env();
    }
    if (!contains(functionTypes, "s3") || !contains(functionTypes,"generic")){
      if (!contains(functionTypes, "primitive")) {
        robj = RObject(ReplaceBody(name, obj));
        Rcout << "RCapturing - " << functionName << endl; 
      } else {
        robj = RObject(DecorateBody(name, obj));
        Rcout << "DCapturing - " << functionName << endl; 
      }
      decorationChanges.insert(pair<string, SEXP>(functionName, obj));
      robj.attr("decorated") = true;
      envir.assign(functionName, robj);
      envir_namespace.assign(functionName, robj);  
    }
    envir.lockBinding(functionName);
    envir_namespace.lockBinding(functionName);
    return true;
  } else {
    warning(functionName + " was already decorated!");
    return false;
  }
}

// [[Rcpp::export]]
bool UndecorateCpp(CharacterVector name){
  string functionName = as<string>(name[0]);
  Environment envir;
  
  Function warning("warning");
  if (decorationChanges.find(functionName) != decorationChanges.end()){
    envir = getFunctionEnvironmentName(functionName);
    envir.unlockBinding(functionName);
    envir.assign(functionName, decorationChanges[functionName]);
    envir.lockBinding(functionName);
  } else {
    warning("Function was not decorated");
  }
  return true;
}

