#include <Rcpp11>
using namespace Rcpp;
using namespace std;

List force_dots(Dots& dots ){
  int n = dots.size() ;
  List out(n) ;
    for( int i=0; i<n; i++){
        out[i] = Rcpp_eval( dots.promise(i), dots.environment(i)) ;    
    }
    return out ;
}

// [[Rcpp::export]]
SEXP GetArgs(Environment evalFrame, List missingArgs, Environment dotsEnv){
  List args;
//  Dots dotsArgs = new Dots(dotsEnv);
  CharacterVector envNames = dotsEnv.ls(false);
  int n = envNames.length();
  for( int i=0; i<n; i++){
    string en = as<string>(envNames[i]);
    if (en != "missingArgs" && !as<bool>(missingArgs[en])){
      SEXP a = dotsEnv.get(en);
      SEXP ev;
      try{
        ev = Rcpp_eval(a, evalFrame);
      } catch(...) {
        ev = a;
      }
      args[en] = ev;
    }
  }
  if (dotsEnv.exists("...")){
    SEXP dots = dotsEnv.get("...");
    vector<SEXP> promises;
    n = 0;
    if( dots != R_MissingArg ){ 
      while(dots != R_NilValue){
        promises.push_back(CAR(dots)) ;
        dots = CDR(dots);
        n++;
      }
    }
    SEXP evalA;
    int k = args.length();
    for( int i=0; i<n; i++){
      SEXP pr = promises[i];
      try{
        evalA = Rcpp_eval( promises[i], dotsEnv);  
      } catch (...){
        if (TYPEOF(pr) == PROMSXP)
          evalA = PRCODE(pr);
        else
          evalA = pr;
      }
      args[i + k] = evalA;   
    }
  }
  return args;
}