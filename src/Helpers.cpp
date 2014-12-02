#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

bool contains(CharacterVector v, string elem){
  for (int i = 0; i < v.length(); i++){
    if (elem == as<string>(v[i]))
    return true;
  } 
  return false;
}

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
  for (int i = 0; i < packages.length(); i++){
    envir = Environment(as<std::string>(packages[i]));
    if (envir.exists(functionName)) {
      obj = envir.get(functionName);
      robj = RObject(obj);
      envir_name = as<std::string>(packages[i]);
      break;
    }
  }
  if (!robj.hasAttribute("decorated")){
    if (envir_name != ".GlobalEnv"){
      string namespace_name = envir_name.substr(8, string::npos);
      envir_namespace = Environment::namespace_env(namespace_name);
      envir_namespace.unlockBinding(functionName);
    } else {
      envir_namespace = Environment::global_env();
    }
    if (!contains(functionTypes, "s3")){
      if (!contains(functionTypes, "primitive")) {
        robj = RObject(ReplaceBody(name, obj));
        Rcout << "RCapturing - " << functionName << endl; 
      } else {
        robj = RObject(DecorateBody(name, obj));
        Rcout << "DCapturing - " << functionName << endl; 
      }
      robj.attr("decorated") = true;
      envir_namespace.assign(functionName, robj);  
    }
    envir_namespace.lockBinding(functionName);
    return true;
  } else {
    warning("Already decorated!");
    return false;
  }
}

