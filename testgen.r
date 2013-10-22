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

# seed.file: the trace file containing all captures
# output.dir: the overall directory to store all outputs. Each run of this function
#   will create its own subdir under output.dir in format of "2013-09-12 15:22:23".
testgen <- function(seed.file, output.dir) {
  if (missing(seed.file)) stop("A seed file must be provided!");
  if (missing(output.dir)) stop("A output directory must be provided!");
  ### make file and path names absolute ###
  abs.path <- function(path) { substring(path,1,1) == .Platform$file.sep; }
  if (!abs.path(seed.file)) seed.file <- file.path(getwd(), seed.file, fsep=.Platform$file.sep);
  if (!file.exists(seed.file)) stop("Cannot find seed file: ", seed.file);
  if (!abs.path(output.dir)) output.dir <- file.path(getwd(), output.dir, fsep=.Platform$file.sep);
  if (!file.exists(output.dir)) stop("Cannot find output directory: ", output.dir);
  output.lnk <- file.path(output.dir, "last",     fsep=.Platform$file.sep);
  output.dir <- file.path(output.dir, Sys.time(), fsep=.Platform$file.sep);
  if (!dir.create(output.dir)) stop("Unable to create directory: ", output.dir);
  if (file.exists(output.lnk)) file.remove(output.lnk);
  if (!file.symlink(output.dir, output.lnk)) stop("Unable to create directory link: ", output.lnk);
  bad.argv.file <- file.path(output.dir, "bad_arguments", fsep=.Platform$file.sep);
  ### process the seed file ###
  map.func.info <- process(readLines(seed.file));
  ### statistic stuff ####
  stat.builtin.total <- length(ls(baseenv()));
  stat.builtin.captured <- length(ls(map.func.info));
  summary.df <- data.frame('TstGen'=vector(), 'BadArg'=vector(), stringsAsFactors=FALSE);
  #### generate test cases ####
  for (func in ls(map.func.info)) {
    stat.tst.gen <- 0;
    stat.bad.arg <- 0;
    #### create testcase file ####
    tc.name.body = func;
    if (tc.name.body == .Platform$file.sep) tc.name.body <- "";
    tc.name <- file.path(output.dir, paste("tc_", tc.name.body, ".r", sep=""), fsep=.Platform$file.sep);
    if (!file.create(tc.name)) stop("Unable to create file: ", tc.name);
    #### generate testcase ####
    info <- map.func.info[[func]];
    for (argv in ls(info$'argvs')) {
      res <- singlegen(func, info$'type', argv, info$'argvs'[[argv]]);
      #### see what we get ####
      if (res$type == "err") {
        #### the captured information is not usable ####
        stat.bad.arg = stat.bad.arg + 1;
        write(res$msg, file=bad.argv.file, append=TRUE);
      } else if (res$type == "src") {
        #### good, we get the source code ####
        stat.tst.gen = stat.tst.gen + 1;
        write(res$msg,  file=tc.name, append=TRUE);
      } else {
        stop("Not reached!");
      }
    }
    func.df <- data.frame('TstGen'=stat.tst.gen, 'BadArg'=stat.bad.arg, row.names=c(func), stringsAsFactors=F);
    summary.df <- rbind(summary.df, func.df);
  }
  summary.df["Total" ,] <- colSums(summary.df);
  cat("========--------   Statistics   --------========\n");
  cat("\n");
  cat("* builtin captured:   ", stat.builtin.captured, " / ", stat.builtin.total, "\n", sep="");
  cat("\n");
  print(summary.df);
  cat("\n");
  cat("=================================================\n");
}

process <- function(lines) {
  lines <- c(lines, "func: dummy");
  funcIdxLst <- grep("^func: ", lines);
  if (length(funcIdxLst) == 1) return(map.func.info);
  process.entry <- function(entry.lines) {
    merge.lines <- function(prefix) {
      pattern <- paste("^", prefix, sep="");
      substr.s <- nchar(prefix)+1;
      item.lines <- entry.lines[grep(pattern, entry.lines)];
      item.lines <- substring(item.lines, substr.s);
      paste(item.lines, collapse='');
    }
    list('func'=merge.lines('func: '),
         'type'=merge.lines('type: '),
         'argv'=merge.lines('args: '),
         'retn'=merge.lines('retn: '));
  }
  map.func.info <- new.env(); # a hashtable: [ func => type | map(args => retn) ]
  for (i in 1:(length(funcIdxLst)-1)) {
    entry <- process.entry(lines[funcIdxLst[i]:(funcIdxLst[i+1]-1)]);
    # use hashtable to store arguments for auto duplication removal
    info <- map.func.info[[entry$'func']];
    if (is.null(info)) {
      info <- list('type'=entry$'type', 'argvs'=new.env());
      map.func.info[[entry$'func']] <- info;
    }
    info$'argvs'[[entry$'argv']] <- entry$'retn';
  }
  map.func.info;
}

singlegen <- function(func, type, argv, retn) {
  # check to see if it is bad arguments
  qualityCheck <- function(what) { tryCatch({eval(parse(text=what))}, warning=print, error=function(e){NULL}); }
  argv.obj <- qualityCheck(argv);
  retn.obj <- qualityCheck(retn);
  valid.argv <-  is.list(argv.obj) || (is.null(argv.obj) && argv == "NULL");
  valid.retn <- !is.null(retn.obj) || (is.null(retn.obj) && retn == "NULL");
  # proper argument should always be packed in a list
  if (!valid.argv || !valid.retn) { 
    return (list(type="err", msg=paste("func:", func, "\ntype:", type, "\nargv:", argv,"\nretn:", retn,"\n")));
  }  
  # TODO: potentially good arguments, alter it 
  # argv.obj.lst <- alter.arguments(argv.obj);
  args <- length(argv.obj); 
  genPrimitive <- function() {
    src <- "";
    if (args > 0) { src <- paste(src, "argv <- eval(parse(text=", deparse(argv), "));", "\n", sep=""); }
    else          { src <- paste(src, "argv <- list();", "\n", sep=""); }
    src <- paste(src, "do.call(\"", func, "\", argv);", sep="");
    src;
  }
  genInternal <- function() {
    src <- "";
    if (args > 0) {
      src <- paste(src, "argv <- eval(parse(text=", deparse(argv), "));", "\n", sep="");
      src <- paste(src, ".Internal(", func, "(",  sep="");
      src <- paste(src, "argv[[1]]",  sep="");
      if (args > 1) { for (idx in 2:args) { src <- paste(src, ", argv[[",idx,"]]", sep=""); } }
      src <- paste(src, "));", "\n", sep="");
    } else {
      src <- paste(src, ".Internal(", func, "());", sep="");
    }
    src;
  }
  code <- switch(type, "P"=genPrimitive(), "I"=genInternal());
  # wrap the test source in the way that harness expected
  src <- "";
  src <- paste(src, "expected <- eval(parse(text=", deparse(retn), "));\n",  sep="");
  src <- paste(src, "test(id=0, code={\n", code, "\n}, o=expected);\n", sep="")
  list(type="src", msg=src);
}

alter.arguments <- function(argv) {
## TODO  
}
