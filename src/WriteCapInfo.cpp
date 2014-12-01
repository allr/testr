#include <Rcpp.h>
#include <R.h>
#include <iostream>
#include <fstream>
#include <sys/stat.h>
#define MAX_FILE_SIZE 50 * 1000000
using namespace Rcpp;
using namespace std;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar
std::string kSymbPrefix = "symb: ";
std::string kValSPrefix = "vsym: ";
std::string kFuncPrefix = "func: ";
std::string kBodyPrefix = "body: ";
std::string kTypePrefix = "type: ";
std::string kArgsPrefix = "argv: ";
std::string kRetvPrefix = "retv: ";
std::string kErrsPrefix = "errs: ";
std::string kWarnPrefix = "warn: ";

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);
 
/* Information for Deparsing Expressions */
typedef enum {
    PP_INVALID  =  0,
    PP_ASSIGN   =  1,
    PP_ASSIGN2  =  2,
    PP_BINARY   =  3,
    PP_BINARY2  =  4,
    PP_BREAK    =  5,
    PP_CURLY    =  6,
    PP_FOR      =  7,
    PP_FUNCALL  =  8,
    PP_FUNCTION =  9,
    PP_IF   = 10,
    PP_NEXT   = 11,
    PP_PAREN    = 12,
    PP_RETURN   = 13,
    PP_SUBASS   = 14,
    PP_SUBSET   = 15,
    PP_WHILE 	= 16,
    PP_UNARY 	= 17,
    PP_DOLLAR 	= 18,
    PP_FOREIGN 	= 19,
    PP_REPEAT 	= 20
} PPkind;
 
typedef enum {
    PREC_FN	 = 0,
    PREC_LEFT    = 1,
    PREC_EQ	 = 2,
    PREC_RIGHT	 = 3,
    PREC_TILDE	 = 4,
    PREC_OR	 = 5,
    PREC_AND	 = 6,
    PREC_NOT	 = 7,
    PREC_COMPARE = 8,
    PREC_SUM	 = 9,
    PREC_PROD	 = 10,
    PREC_PERCENT = 11,
    PREC_COLON	 = 12,
    PREC_SIGN	 = 13,
    PREC_POWER	 = 14,
    PREC_DOLLAR  = 15,
    PREC_NS	 = 16,
    PREC_SUBSET	 = 17
} PPprec;
 
typedef struct {
	PPkind kind; 	 /* deparse kind */
	PPprec precedence; /* operator precedence */
	unsigned int rightassoc;  /* right associative? */
} PPinfo;
 
typedef struct {
    char   *name;    /* print name */
    CCODE  cfun;     /* c-code address */
    int	   code;     /* offset within c-code */
    int	   eval;     /* evaluate args? */
    int	   arity;    /* function arity */
    PPinfo gram;     /* pretty-print info */
} FUNTAB;
 
extern FUNTAB	R_FunTab[];	    /* Built in functions */ 
 
CCODE get_internal( std::string name){
    FUNTAB* p = R_FunTab ;
    for( ; p->name != NULL; ++p ){
//        Rprintf( "%s\n", p->name ) ;
        if( name == p->name )
            return p->cfun ;
    }
    return NULL ;
}
 
SEXP deparse( SEXP x){
      CCODE deparse_fun = get_internal("deparse");
      Language call("deparse", x) ;    
      return deparse_fun(call, Rf_ScalarInteger(0), CDR(call), R_GlobalEnv ) ; 
}

std::ofstream tracefile;
  
void printCapture(CharacterVector x, std::string prefix) {
      if (x[0] != "NULL"){
        if (x.length() < 100) {
          for (int i = 0; i < x.length(); i++)
            tracefile << prefix << x[i] << std::endl;
        } else {
            tracefile << prefix << "<too long>" << std::endl;
        }
      }
}

int captureFileNumber = 0;

// [[Rcpp::export]]
void WriteCapInfo_cpp (CharacterVector fname, List args, SEXP retv, SEXP errs, SEXP warns) {
  Environment testr = Environment::namespace_env("testr");
  Environment cache = testr.get("cache");
  string traceFile = as<string>(cache.get("trace.folder.path"));
  traceFile += "/";
  traceFile += as<string>(testr.get("kCaptureFile"));
  traceFile += "."; 
  traceFile += to_string(captureFileNumber);
  tracefile.open(traceFile.c_str(), std::ios::app);
  printCapture(fname, kFuncPrefix);
  printCapture(deparse(args), kArgsPrefix);
  printCapture(deparse(warns), kWarnPrefix);
  printCapture(deparse(retv), kRetvPrefix);
  printCapture(deparse(errs), kErrsPrefix);
  tracefile << std::endl;
  tracefile.close();
  // get file size
  struct stat stat_buf;
  int rc = stat(traceFile.c_str(), &stat_buf);
  if (stat_buf.st_size > MAX_FILE_SIZE)
    captureFileNumber++;
}

