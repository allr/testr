#include <Rcpp11>
#include "testr.h"
using namespace std;
using namespace Rcpp;
map<string, SEXP> decorationChanges;

// [[Rcpp::export]]
bool DecorateSubst_cpp(CharacterVector name, CharacterVector functionTypes) {
  Function DecorateBody("DecorateBody");
  Function ReplaceBody("ReplaceBody");
  Environment testr("package:testr");
  SEXP obj;
  Environment envir_namespace;
  string envir_name;
  
  Environment envir;
  Function warning("warning");
  string functionName = as<std::string>(name[0]); 
  envir_name = getFunctionEnvironmentName(functionName);
  envir = Environment(envir_name);
  obj = envir.get(functionName);
  RObject robj(obj);
  std::cout << functionName << endl;
  SEXP decAttr = robj.attr("decorated");
  if (decAttr == NULL){
    if (envir_name != ".GlobalEnv"){
      string namespace_name = envir_name.substr(8, string::npos);
      envir_namespace = Environment::namespace_env(namespace_name);
      envir_namespace.unlockBinding(functionName);
    } else {
      envir_namespace = Environment::global_env();
    }
    if (!contains(functionTypes, "s3") || !contains(functionTypes, "generic") ){
      if (!contains(functionTypes, "primitive")) {
        robj = RObject(ReplaceBody(name, obj));
        std::cout << "RCapturing - " << functionName << endl; 
        decorationChanges.insert(pair<string, SEXP>(functionName, obj));
//        robj.attr("decorated") = true;
        envir_namespace.assign(functionName, robj);  
      } else {
//        robj = RObject(DecorateBody(name, obj));
//        Rcout << "DCapturing - " << functionName << endl; 
      }
    }
//    envir_namespace.lockBinding(functionName);
    return true;
  } else {
    warning("Already decorated!");
    return false;
  }
}

//map<string, SEXP> funDec;
//map<string, Environment> funEnv;
//void fullDecorate(CharacterVector functions){
//  
//}


// [[export]]
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

