#include "testr.h"
using namespace Rcpp;
using namespace std;

SEXP GetArgs(SEXP dotsE){
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
    SEXP nameSym = Rf_install(name.c_str());
    unevaluatedArg = Rf_findVar(nameSym, dotsE);
    if (missing(nameSym, dotsE)) {
      continue;
    }

    if (unevaluatedArg != R_UnboundValue) {
        if(TYPEOF(unevaluatedArg) == PROMSXP) {
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
          args[name] = evaluatedArg;
        } else {
          // Non promises such as vectors or other constants
          // are already evaluated.
          args[name] = unevaluatedArg;
        }
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
