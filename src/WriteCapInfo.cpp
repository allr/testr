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

// [[Rcpp::export]]
std::string dput_c(SEXP x) {
  std::string result;
  if (x == R_NilValue) 
    return "";
  if (TYPEOF(x) == LISTSXP || TYPEOF(x) == VECSXP) {
    List argsL(x);
    // problem is here
    List names = argsL.names();
    result += "list(";
    for (int i = 0; i < argsL.size(); i++) {
      if (names.size() > i) {
        std::string name = as<std::string>(names[i]);
        if (name != ""){
          result += name;
          result += "=";
        }
      }
      result += dput_c(argsL[i]);
      if (i != argsL.size() - 1) 
      result +=",\n";
    }
    result += ");";
  } else if (Rf_isS4(x)) {
    Rcpp::S4 obj(x);
    std::vector<std::string> snames= obj.attributeNames();
    result+= "new(\"" + as<std::string>(obj.attr("class")) + "\"\n";
    for (int i = 0; i < snames.size(); i++) {
      result += ",\n" + snames[i] + "=";
      result += dput_c(obj.slot(snames[i]));
    }
    result+= ")";
  } else {
    if (TYPEOF(x) == LANGSXP) 
      result += "quote(";
    CharacterVector dx = deparse(x); 
    if (dx.length() < 1000) 
      for (int i = 0; i < dx.length(); i++) 
        result += as<std::string>(dx[i]);
    else 
        result += "<too long>";
    if (TYPEOF(x) == LANGSXP) 
      result += ")";
  }
  return result;
}

int captureFileNumber = 0;

//// [[Rcpp::export]]
//void WriteCapInfo_cpp (CharacterVector fname, SEXP args_env) {
//  Environment testr = Environment::namespace_env("testr");
//  Environment cache = testr.get("cache");
//  string traceFile = as<string>(cache.get("trace.folder.path"));
//  traceFile += "/";
//  traceFile += as<string>(testr.get("kCaptureFile"));
//  traceFile += "."; 
//  char numstr[21];
//  sprintf(numstr, "%d", captureFileNumber);
//  traceFile += numstr;
//  tracefile.open(traceFile.c_str(), std::ios::app);
//  tracefile << kFuncPrefix << fname[0] << endl;
//  //  printCapture(GetArgs(args_env), kArgsPrefix);
//  tracefile << std::endl;
//  tracefile.close();
//  // get file size
//  struct stat stat_buf;
//  stat(traceFile.c_str(), &stat_buf);
//  if (stat_buf.st_size > MAX_FILE_SIZE)
//  captureFileNumber++;
//}

