#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

List force_dots(Environment env){
  List promises;
  int n = 0;
  SEXP dots = env.find("...") ;
  if( dots != R_MissingArg ){ 
    while(dots != R_NilValue){
      promises.push_back(CAR(dots)) ;
      dots = CDR(dots);
      n++;
    }
  }
  List out(n) ;
  for( int i=0; i<n; i++){
    out[i] = Rcpp_eval( promises(i), env) ;    
  }
  return out ;
}

// [[Rcpp::export]]
SEXP GetArgs(List missingArgs, SEXP dotsE){
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
      SEXP nameSym = Rf_install(name.c_str());
      unevaluatedArg = Rf_findVarInFrame( dotsE, nameSym );
      if (unevaluatedArg != R_UnboundValue && TYPEOF(unevaluatedArg) == PROMSXP) {
        if (!Rf_isNull(PRENV(unevaluatedArg))){
          evalEnv = PRENV(unevaluatedArg);
        } else {
          evalEnv = dotsE;
        }
        SEXP x = Rf_lang3( Rf_install("try"), unevaluatedArg, Rf_ScalarLogical(TRUE) );
        SET_TAG( CDDR(x), Rf_install("silent") );
        SEXP res = Rf_eval( x, evalEnv) ;
        if( inherits( res, "try-error" ) ){
            Rcout << "YAY!" << endl;
            evaluatedArg = unevaluatedArg;
        } else 
        {
          evaluatedArg = res;
        }
//        Promise pr(unevaluatedArg);

//        try{
//          evaluatedArg = Rcpp_eval(unevaluatedArg, evalEnv);
//        } catch(...) {
////          evaluatedArg = Rf_eval(unevaluatedArg, dotsE);
//          evaluatedArg = PRCODE(unevaluatedArg);        
//        }
          
      } else {
        evaluatedArg = dotsEnv.get(name);
      }
      
      args[name] = evaluatedArg;
    }
  }
  if (dotsEnv.exists("...")){
    SEXP dots = dotsEnv.get("...");
    vector<SEXP> promises;
    nArgs = 0;
    if( dots != R_MissingArg ){ 
      while(dots != R_NilValue){
        promises.push_back(CAR(dots)) ;
        dots = CDR(dots);
        nArgs++;
      }
    }
    for( int i=0; i<nArgs; i++){
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
      args.push_back(evaluatedArg);
    }
  }
  return args;
}