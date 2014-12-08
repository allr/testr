typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);
 
CCODE get_internal(std::string);
SEXP search();
bool contains(Rcpp11::CharacterVector, std::string);
std::string getFunctionEnvironmentName(std::string &functionName);
SEXP deparse(SEXP);