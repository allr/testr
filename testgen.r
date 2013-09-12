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
  DEBUG <- TRUE;
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
  bad.args.file <- file.path(output.dir, "bad_arguments", fsep=.Platform$file.sep);
  ### process the seed file ###
  map.func.info <- process(readLines(seed.file));
  ### statistic stuff ####
  stat.builtin.total <- length(ls(baseenv()));
  stat.builtin.captured <- length(ls(map.func.info));
  summary.df <- data.frame('TstGen'=vector(), 'BadArg'=vector(), stringsAsFactors=FALSE);
  #### generate test cases ####
  tc.name.prefix <- "tc_";
  for (func in ls(map.func.info)) {
    stat.tst.gen <- 0;
    stat.bad.arg <- 0;
    #### create testcase file ####
    tc.name.body = func;
    if (tc.name.body == .Platform$file.sep) tc.name.body <- "";
    tc.name <- file.path(output.dir, paste(tc.name.prefix, tc.name.body, sep=""), fsep=.Platform$file.sep);
    if (!file.create(tc.name)) stop("Unable to create testcase file: ", tc.name);
    #### generate testcase ####
    info <- map.func.info[[func]];
    for (args in info$'argss') {
      res <- singlegen(func, info$'type', args);
      #### see what we get ####
      if (res$type == "err") {
        #### the captured information is not usable ####
        stat.bad.arg = stat.bad.arg + 1;
        write(res$msg, file=bad.args.file, append=TRUE);
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
  DEBUG <- TRUE;
  lines <- c(lines, "xxxx: dummy");
  map.func.info <- new.env(); # a hashtable mapping function name to its args
  funcIdxLst <- grep("^func: ", lines);
  if (length(funcIdxLst) == 0) return(map.func.info);
  contentS <- nchar("func: ")+1;
  lines <- substring(lines, contentS); # removing prefixes: "func: ", "type: ", or "args: "
  for (i in 1:(length(funcIdxLst)-1)) {
    func <- lines[funcIdxLst[i]];
    type <- lines[funcIdxLst[i]+1];
    argS <- funcIdxLst[i]+2;
    argE <- funcIdxLst[i+1]-1;
    if (DEBUG && FALSE) { cat(i, func, type, argS, argE, "\n", sep=" - "); }
    args <- paste(lines[argS:argE], collapse='');
    # use hashtable to store arguments for auto duplication removal
    info <- map.func.info[[func]];
    if (is.null(info)) {
      info <- list('type'=type, 'argss'=new.env());
      map.func.info[[func]] <- info;
    }
    info$'argss'[[args]] <- TRUE;
  }
  for (f in ls(map.func.info)) {
    map.func.info[[f]]$'argss' <- ls(map.func.info[[f]]$'argss');
  }
  if (DEBUG && FALSE) {
    for (f in ls(map.func.info)) {
      cat("func:", f, "\n");
      cat("type:", map.func.info[[f]]$'type', "\n");
      cat(paste("argv:", map.func.info[[f]]$'argss', "\n"), sep="");
    }
  }
  map.func.info;
}

singlegen <- function(func, type, argv) {
  DEBUG <- TRUE;
  if (DEBUG && FALSE) {
    cat("func:", func, "\n");
    cat("type:", type, "\n");
    cat("argv:", argv, "\n");
  }
  # not a valid argument list, possible reason is arguments too long therefore ignored.
  argv1 <- tryCatch({
    eval(parse(text=argv));
  }, warning = function(war) {
    print(war); #TODO: why warninng?
  }, error = function(err) {
    # cat("[BAD_ARGS]", argv, "\tfunc:", func, "\n");
  });
  if (!is.list(argv1)) {
    return (list(type="err", msg=paste("func:", func,"\nargv:", argv,"\n")));
  }
  args1 <- length(argv1); 
  genPrimitive <- function() {
    src <- "";
    if (args1 > 0) {
      src <- paste(src, "argv <- eval(parse(text=\'", argv, "\'));", "\n", sep="");
    } else {
      src <- paste(src, "argv <- list();", "\n", sep="");
    }
    src <- paste(src, "do.call(\"", func, "\", argv);", "\n", sep="");
    src;
  }
  genInternal <- function() {
    src <- "";
    if (args1 > 0) {
      src <- paste(src, "argv <- eval(parse(text=\'", argv, "\'));", "\n", sep="");
      src <- paste(src, ".Internal(", func, "(",  sep="");
      src <- paste(src, "argv[[1]]",  sep="");
      if (args1 > 1) { for (idx in 2:args1) { src <- paste(src, ", argv[[",idx,"]]", sep=""); } }
      src <- paste(src, "));", "\n", sep="");
    } else {
      src <- paste(src, ".Internal(", func, ");", "\n", sep="");
    }
    src;
  }
  src <- switch(type, "P"=genPrimitive(), "I"=genInternal());
  list(type="src", msg=src);
}

