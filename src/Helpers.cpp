#include <Rcpp.h>
using namespace std;

bool contains(Rcpp::CharacterVector v, string elem){
  for (int i = 0; i < v.length(); i++){
    if (elem == Rcpp::as<string>(v[i]))
      return true;
  } 
  return false;
}
// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
void DecorateSubst_cpp(Rcpp::CharacterVector packages, Rcpp::CharacterVector name, bool captureGenerics, 
   Rcpp::CharacterVector functionTypes, Rcpp::CharacterVector primGenerics, Rcpp::CharacterVector prim) {
   Rcpp::Function DecorateBody("DecorateBody");
   Rcpp::Function ReplaceBody("ReplaceBody");
   Rcpp::Environment testr("package:testr");
   string functionName = Rcpp::as<std::string>(name[0]); 
   SEXP obj;
   Rcpp::Environment env;
   string envir_name;
   Rcpp::Environment envir;
//   Rcout << "Trying to capture - " << functionName << endl;
   for (int i = 0; i < packages.length(); i++){
     envir = Rcpp::Environment(Rcpp::as<std::string>(packages[i]));
     bool b = envir.exists(functionName);
     if (b) {
        obj = envir.get(functionName);
        env = envir;
        envir_name = Rcpp::as<std::string>(packages[i]);
     }
   }
   Rcpp::Environment x;
   if (envir_name != ".GlobalEnv"){
      string namespace_name = envir_name.substr(8, string::npos);
      x = Rcpp::Environment::namespace_env(namespace_name);
      x.unlockBinding(functionName);
   } else {
      x = Rcpp::Environment::global_env();
   }
   if (captureGenerics && (contains(functionTypes, "generic") || contains(prim, functionName))){
//      x.assign(functionName, DecorateBody(name, obj));  
   } else {
      x.assign(functionName, ReplaceBody(name, obj));  
      Rcpp::Rcout << "Capturing - " << functionName << endl; 
   }
   x.lockBinding(functionName);
}

