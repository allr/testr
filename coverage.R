# Copyright (c) 2013, Purdue University. All rights reserved.
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
#
# This code is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 only, as
# published by the Free Software Foundation.
#
# This code is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# version 2 for more details (a copy is included in the LICENSE file that
# accompanied this code).
#
# You should have received a copy of the GNU General Public License version
# 2 along with this work; if not, write to the Free Software Foundation,
# Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
# or visit www.oracle.com if you need additional information or have any
# questions.

#' Coverage Report
#'
#' Parameters:
#'  * root: a directory or a single C file that contains/is instrumented VM source.
#'          The VM must have been compiled with gcov support and executed at least once.
#'  * exclude.header: whether include header files in statistics.
#'  * file/func.detail: whether show detailed tables.
#'  * file/func.keyword: only include the files/functions that contain the specified keyword in statistics.
#'  * ignore.case: whether case should be ignored for the keywords.
#' Return: list(file=file.df, func=func.df)
#'

coverage <- function(root, exclude.header=TRUE, file.detail=FALSE, func.detail=FALSE, file.keyword="", func.keyword="", ignore.case=TRUE, ...) {
  DEBUG <- FALSE;
  if (missing(root)) stop("A directory containing VM source files must be specified!");
  if (DEBUG) cat("===",root,"===","\n");
  if (DEBUG) cat("===",root,"===","\n");
  if (length(grep("[.]c$", root, ignore.case=TRUE))) { cfiles <- root }
  else { cfiles <- list.files(path=root, recursive=TRUE, pattern=".c$") }
  if (DEBUG) print(cfiles);
  res <- c()
  gcovOnSingleFile <- function(f) {
    if (DEBUG) cat("-----------------",f,"-----------------","\n");
    path <- file.path(root, dirname(f), fsep=.Platform$file.sep);
    file <- file.path(root, f,          fsep=.Platform$file.sep);
    cmd <- paste("gcov", "-p", "-n", "-f",  "-o", path, file, sep=" ");
    r <- system(cmd, intern=TRUE, ignore.stderr=TRUE);
    fileLines <- r[grep("^File", r)];
    funcLines <- r[grep("^Function", r)];
    dataLines <- r[grep("^Lines executed", r)];
    if (DEBUG) print(fileLines);
    if (DEBUG) print(funcLines);
    if (DEBUG) print(dataLines);
    if (length(fileLines) != 0) {
      files.len <- length(fileLines);
      funcs.len <- length(funcLines);
      data.len  <- length(dataLines);
      if (files.len + funcs.len != data.len) stop("Unexpected gcov output format!");
      files <- matrix(unlist(strsplit(fileLines,"'")), ncol=2, byrow=TRUE)[,2];
      funcs <- matrix(unlist(strsplit(funcLines,"'")), ncol=2, byrow=TRUE)[,2];
      data  <- strsplit(unlist(strsplit(dataLines, "Lines executed:")), "% of ")[seq(2,2*data.len,2)];
      if (DEBUG) print(files);
      if (DEBUG) print(funcs);
      if (DEBUG) print(data);
      func.data <- data[1:funcs.len];
      file.data <- data[(funcs.len+1):data.len];
      if (DEBUG) print(func.data);
      if (DEBUG) print(file.data);
      func.df <- data.frame(cbind(funcs, matrix(unlist(func.data), ncol=2, byrow=TRUE)), stringsAsFactors=FALSE);
      file.df <- data.frame(cbind(files, matrix(unlist(file.data), ncol=2, byrow=TRUE)), stringsAsFactors=FALSE);
      colnames(func.df) <- c("Func", "CovLn%", "LOC");
      colnames(file.df) <- c("File", "CovLn%", "LOC");
      calCovLnAndAddObj <- function(df) {
        covLn <- round(as.numeric(df$'CovLn%') / 100.0 * as.numeric(df$'LOC'), digits=0);
        df <- cbind(df, 'CovLn'=covLn);
        cbind(df, Obj=f);        
      }
      func.df <- calCovLnAndAddObj(func.df)[,c(5,1,4,3,2)]; # Obj Func CovLn LOC CovLn%
      file.df <- calCovLnAndAddObj(file.df)[,c(5,1,4,3,2)]; # Obj File CovLn LOC CovLn%
      if (DEBUG) print(func.df);
      if (DEBUG) print(file.df);
      list(file=file.df, func=func.df);
    } else { NULL }
  }
  result <- Map(gcovOnSingleFile, cfiles);
  if (DEBUG) print(result);
  file.df <- data.frame('File'=vector(),'CovLn%'=vector(),'LOC'=vector(),'CovLn'=vector(),'Obj'=vector());
  func.df <- data.frame('Func'=vector(),'CovLn%'=vector(),'LOC'=vector(),'CovLn'=vector(),'Obj'=vector());
  for (item in result) {
    file.df <- rbind(file.df, item$file);
    func.df <- rbind(func.df, item$func);
  }
  if (DEBUG) print(file.df);
  if (DEBUG) print(func.df);
  file.df <- file.df[grep(file.keyword, file.df$'Obj',  ignore.case=ignore.case),];
  func.df <- func.df[grep(file.keyword, func.df$'Obj',  ignore.case=ignore.case),];
  func.df <- func.df[grep(func.keyword, func.df$'Func', ignore.case=ignore.case),];
  if (exclude.header) {
    file.df <- file.df[grep("[.]c$", file.df$'File', ignore.case=ignore.case),];  
  }
  if (DEBUG) print(file.df);
  if (DEBUG) print(func.df);
  # line file coverage
  totalLine.file        <- sum(as.numeric(file.df$'LOC'));
  totalCovLine.file     <- sum(as.numeric(file.df$'CovLn'));
  totalCovLinePcnt.file <- round(totalCovLine.file / totalLine.file * 100, digits=2);
  # line func coverage
  totalLine.func        <- sum(as.numeric(func.df$'LOC'));
  totalCovLine.func     <- sum(as.numeric(func.df$'CovLn'));
  totalCovLinePcnt.func <- round(totalCovLine.func / totalLine.func * 100, digits=2);
  # func coverage
  totalFunc        <- nrow(func.df);
  totalCovFunc     <- nrow(func.df[as.numeric(func.df$'CovLn')>0,]);
  totalCovFuncPcnt <- round(totalCovFunc / totalFunc * 100, digits=2);
  # file coverage
  totalFile        <- nrow(file.df);
  totalCovFile     <- nrow(file.df[as.numeric(file.df$'CovLn')>0,]);
  totalCovFilePcnt <- round(totalCovFile / totalFile * 100, digits=2);

  cat("========-------- Coverage Report --------========\n");
  cat("\n");
  cat(">>> Configration:\n");
  cat("\n");
  cat("- src root:         ", root,           "\n", sep="");
  cat("- file keyword:     ", file.keyword,   "\n", sep="");
  cat("- func keyword:     ", func.keyword,   "\n", sep="");
  cat("- igore case:       ", ignore.case,    "\n", sep="");
  cat("- exclude header:   ", exclude.header, "\n", sep="");
  cat("\n");
  cat(">>> Coverage:\n");
  cat("\n");
  cat("* Line (file): ", totalCovLine.file, " out of ", totalLine.file, " (", totalCovLinePcnt.file, "%)\n", sep="");
  cat("* Line (func): ", totalCovLine.func, " out of ", totalLine.func, " (", totalCovLinePcnt.file, "%)\n", sep="");
  cat("* File:        ", totalCovFile,      " out of ", totalFile,      " (", totalCovFilePcnt,      "%)\n", sep="");
  cat("* Func:        ", totalCovFunc,      " out of ", totalFunc,      " (", totalCovFuncPcnt,      "%)\n", sep="");
  if (file.detail) {
    cat("\n");
    cat("----------------   File Detail   ----------------\n");
    cat("\n");
    print(file.df, row.names=FALSE);
  }
  if (func.detail) {
    cat("\n");
    cat("----------------   Func Detail   ----------------\n");
    cat("\n");
    print(func.df, row.names=FALSE);
  }
  cat("\n");
  cat("=================================================\n");
  return (list(file=file.df, func=func.df));
}

#coverage("/home/lzhao/r/gcov/src")
#coverage("/home/lzhao/r/R-3.0.1/src/main")
