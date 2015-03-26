#include "testr.h"
using namespace Rcpp;
using namespace std;

std::vector<SEXP> trace_arguments;

SEXP pop_args() {
  SEXP args = trace_arguments.back();
  trace_arguments.pop_back();
  return args;
}

SEXP GetArgs(SEXP dotsE){
  Environment testr = Environment::namespace_env("testr");
  Environment cache = testr.get("cache");
  LogicalVector wd = cache.get("writing.down");
  bool writingDown = as<bool>(wd);
  if (writingDown){ 
    return R_NilValue;
  }
  List args(0);
  Environment dotsEnv(dotsE);
  CharacterVector envNames = dotsEnv.ls(false);
  int nArgs = envNames.length();
  SEXP evalEnv;
  SEXP unevaluatedArg;
  SEXP evaluatedArg = R_NilValue;
  for( int i=0; i<nArgs; i++){
    evaluatedArg = R_NilValue;
    string name = as<string>(envNames[i]);
//    Rcout << "name - " << name << endl;
    SEXP nameSym = Rf_install(name.c_str());
    unevaluatedArg = Rf_findVarInFrame(dotsE, nameSym);
    if (missing(nameSym, dotsE)) {
      continue;
    }
    if (unevaluatedArg != R_UnboundValue && TYPEOF(unevaluatedArg) == PROMSXP) {
      SEXP prcode = PRCODE(unevaluatedArg);
      if (!Rf_isNull(PRENV(unevaluatedArg))){
//        Rcout << "PREnv is not null" << endl;
        evalEnv = PRENV(unevaluatedArg);
      } else {
        evalEnv = dotsE;
      }
      evalEnv = dotsE;
      int err = 0;
      SEXP res = R_tryEvalSilent(unevaluatedArg, evalEnv, &err);
      if(err){
//        Rcout << "Error in promise eval - " << name << endl;
        evaluatedArg = prcode;
      } else {
        evaluatedArg = res;
      }
//      dotsEnv.assign(name, prcode);

        args[name] = evaluatedArg; 
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
      if (unevaluatedArg != R_UnboundValue && TYPEOF(unevaluatedArg) == PROMSXP) {
        SEXP prcode = PRCODE(unevaluatedArg);
        if (!Rf_isNull(PRENV(unevaluatedArg))){
          evalEnv = PRENV(unevaluatedArg);
        } else {
          evalEnv = dotsE;
        }
        int err = 0;
        SEXP res = R_tryEvalSilent(unevaluatedArg, evalEnv, &err);
        if(err){
          evaluatedArg = prcode;
        } else {
          evaluatedArg = res;
        }
      }
      args.push_back(evaluatedArg);
    }
  }
  return args;
}

// [[Rcpp::export]]
void SaveArgs(SEXP env) {
  SEXP args = GetArgs(env);
  trace_arguments.push_back(args);
}

