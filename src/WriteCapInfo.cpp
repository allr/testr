#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include "testr.h"
#include <sys/stat.h>
#define MAX_FILE_SIZE 50 * 1000000
using namespace Rcpp;
using namespace std;

std::string kSymbPrefix = "symb: ";
std::string kValSPrefix = "vsym: ";
std::string kFuncPrefix = "func: ";
std::string kBodyPrefix = "body: ";
std::string kTypePrefix = "type: ";
std::string kArgsPrefix = "argv: ";
std::string kRetvPrefix = "retv: ";
std::string kErrsPrefix = "errs: ";
std::string kWarnPrefix = "warn: ";

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

