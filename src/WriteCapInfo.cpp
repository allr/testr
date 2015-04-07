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
std::string kArgsPrefix = "argv: ";

std::ofstream tracefile;

void printCapture(SEXP args, std::string prefix) {
  if (args == R_NilValue) 
    return;
  if (TYPEOF(args) == LISTSXP || TYPEOF(args) == VECSXP) {
    List argsL(args);
    // problem is here
    List names = argsL.names();
    tracefile << prefix << "list(" << std::endl;
    for (int i = 0; i < argsL.size(); i++) {
      if (names.size() > i) {
        std::string name = as<std::string>(names[i]);
        if (name != "") 
          tracefile << prefix << name << "=" << std::endl;
      }
      printCapture(argsL[i], prefix);
      if (i != argsL.size() - 1) 
        tracefile << prefix << "," << std::endl;
    }
    tracefile << prefix << ")" << std::endl;
  } else if (Rf_isS4(args)) {
    Rcpp::S4 obj(args);
    std::vector<std::string> snames= obj.attributeNames();
    tracefile << prefix << "new(\"" << as<std::string>(obj.attr("class")) << "\"" << std::endl;
    for (int i = 0; i < snames.size(); i++) {
      SEXP x1 = obj.slot(snames[i]);
      tracefile << prefix << "," << snames[i] << "=" << std::endl;
      printCapture(x1, prefix);
    }
    tracefile << prefix << ")" << std::endl;    
  } 
  else {
    if (TYPEOF(args) == LANGSXP) {
      tracefile << prefix << "quote(" << std::endl;
    }
    CharacterVector x = deparse(args); 
    if (x.length() < 1000) {
      for (int i = 0; i < x.length(); i++) {
        tracefile << prefix << x[i] << std::endl;
      }
    } else {
      tracefile << prefix << "<too long>" << std::endl;
    }
    if (TYPEOF(args) == LANGSXP) {
      tracefile << prefix << ")" << std::endl;
    }
  }
}

int captureFileNumber = 0;

// [[Rcpp::export]]
void WriteCapInfo_cpp (CharacterVector fname, SEXP args_env) {
  Environment testr = Environment::namespace_env("testr");
  Environment cache = testr.get("cache");
  string traceFile = as<string>(cache.get("trace.folder.path"));
  traceFile += "/";
  traceFile += as<string>(testr.get("kCaptureFile"));
  traceFile += "."; 
  char numstr[21];
  sprintf(numstr, "%d", captureFileNumber);
  traceFile += numstr;
  tracefile.open(traceFile.c_str(), std::ios::app);
  tracefile << kFuncPrefix << fname[0] << endl;
  printCapture(GetArgs(args_env), kArgsPrefix);
  tracefile << std::endl;
  tracefile.close();
  // get file size
  struct stat stat_buf;
  stat(traceFile.c_str(), &stat_buf);
  if (stat_buf.st_size > MAX_FILE_SIZE)
  captureFileNumber++;
}

