#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
SEXP GetArgs1(List missingArgs, SEXP dotsE){
  Environment testr = Environment::namespace_env("testr");
  Environment cache = testr.get("cache");
  LogicalVector wd = cache.get("writing.down");
  bool writingDown = as<bool>(wd);
  if (writingDown){
    return R_NilValue;
  }
  
  List args;
  Environment dotsEnv(dotsE);
  CharacterVector envNames = dotsEnv.ls(false);
  int nArgs = envNames.length();
  SEXP evalEnv;
  SEXP unevaluatedArg;
  SEXP evaluatedArg = R_NilValue;
  for( int i=0; i<nArgs; i++){
    evaluatedArg = R_NilValue;
    string name = as<string>(envNames[i]);
    if (name != "missingArgs" && !as<bool>(missingArgs[name])){
        args[name] = dotsEnv.get(name); 
    }
  }
  nArgs--;
  if (dotsEnv.exists("...")){
    SEXP dots = dotsEnv.get("...");
    vector<SEXP> promises;
    int dArgs = 0;
    if( dots != R_MissingArg ){ 
      while(dots != R_NilValue){
        promises.push_back(CAR(dots)) ;
        dots = CDR(dots);
        dArgs++;
      }
    }
    List dotArgs(dArgs);
    for( int i=0; i< dArgs; i++){
      unevaluatedArg = promises[i];
      if (TYPEOF(unevaluatedArg) == PROMSXP) {
        evalEnv = PRENV(unevaluatedArg);
      } else {
        evalEnv = dotsEnv;
      }
      try{
        evaluatedArg = Rcpp_eval(unevaluatedArg, evalEnv);
      } catch(...) {
        if (TYPEOF(unevaluatedArg) == PROMSXP) {
          evaluatedArg = PRCODE(unevaluatedArg);        
        } else {
          evaluatedArg = unevaluatedArg;
        }
      }
      dotArgs[i] = evaluatedArg;
    }
  }
  return args;
}

