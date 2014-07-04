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
testgen <- function(input.dir, output.dir, verbose=FALSE) {
  
  if (missing(input.dir))  stop("A input diretory must be provided!");
  if (missing(output.dir)) stop("A output directory must be provided!");
  
  is.abs.path <- function(path) { c <- substring(path,1,1); c == .Platform$file.sep || c == '~'; }
  mk.abs.path <- function(path) { file.path(getwd(), path, fsep=.Platform$file.sep); }

  ## make file and path names absolute
  if (!is.abs.path(input.dir))  input.dir <- mk.abs.path(input.dir);
  if (!is.abs.path(output.dir)) output.dir <- mk.abs.path(output.dir);
  if (!file.exists(input.dir))  stop("Cannot find input directory: ", input.dir);
  if (!file.exists(output.dir)) stop("Cannot find output directory: ", output.dir);
  
  ## create a new directory to store generated testcases
  output.lnk <- file.path(output.dir, "last",     fsep=.Platform$file.sep);
  output.dir <- file.path(output.dir, Sys.time(), fsep=.Platform$file.sep);
  if (!dir.create(output.dir)) stop("Unable to create directory: ", output.dir);
  if (file.exists(output.lnk)) file.remove(output.lnk);
  if (!file.symlink(output.dir, output.lnk)) stop("Unable to create directory link: ", output.lnk);
  bad.argv.file <- file.path(output.dir, "bad_arguments", fsep=.Platform$file.sep);
  if (!file.create(bad.argv.file)) stop("Unable to create file: ", bad.argv.file);

  ## get the seed files
  seed.files <- list.files(path=input.dir, recursive=TRUE);
  
  if (verbose) {
    cat("Read from", input.dir, "\n");
    cat("Total", length(seed.files), "trace files\n");
    cat("Output to", output.dir, "\n");
  }
  gen.stat.map <- new.env();
  for (sf in seed.files) {
    sf <- file.path(input.dir, sf, fsep=.Platform$file.sep);
    stat <- gen.file(sf, output.dir, bad.argv.file, verbose);
    for (func in ls(stat)) {
      cur.stat <- gen.stat.map[[func]];
      if (is.null(cur.stat)) {
        cur.stat <- c(0,0);
      }
      gen.stat.map[[func]] <- cur.stat + stat[[func]];
    }
  }
  report(gen.stat.map);
}

gen.file <- function(seed.file, output.dir, bad.argv.file, verbose=FALSE) {
  ensure.tc.file <- function(path, name) {
    if (name == .Platform$file.sep) name <- "";
    tc.file <- file.path(path, paste("tc_", name, ".r", sep=""), fsep=.Platform$file.sep);
    if (!file.exists(tc.file) && !file.create(tc.file)) stop("Unable to create file: ", tc.file);
    return(tc.file);
  }
  if (verbose) {
    cat(">>> processing file: ", seed.file, "...", "\n");
  }
  ### read the seed file into a table
  func.info.map <- process(readLines(seed.file));
  #### generate test cases
  func.stat.map <- new.env();
  for (func in ls(func.info.map)) {
    stat.tst.gen <- 0;
    stat.bad.arg <- 0;
    tc.file <- ensure.tc.file(output.dir, func);
    #### generate testcase
    info <- func.info.map[[func]];
    for (argv in ls(info$'argvs')) {
      feedback <- gen.func(func, info$'type', argv, info$'argvs'[[argv]]);
      #### see what we get
      if (feedback$'type' == "err") {
        #### the captured information is not usable
        stat.bad.arg = stat.bad.arg + 1;
        write(feedback$'msg', file=bad.argv.file, append=TRUE);
      } else if (feedback$'type' == "src") {
        #### good, we get the source code
        stat.tst.gen = stat.tst.gen + 1;
        write(feedback$'msg',  file=tc.file, append=TRUE);
      } else {
        stop("Not reached!");
      }
    }
    func.stat.map[[func]] <- c(stat.tst.gen, stat.bad.arg);
  }
  return(func.stat.map);
}

process <- function(lines) {
  lines <- c(lines, "func: dummy");
  funcIdxLst <- grep("^func: ", lines);
  if (length(funcIdxLst) == 1) return(func.info.map);
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
  func.info.map <- new.env(); # hashtable: [ func => type | map(args => retn) ]
  for (i in 1:(length(funcIdxLst)-1)) {
    entry <- process.entry(lines[funcIdxLst[i]:(funcIdxLst[i+1]-1)]);
    # use hashtable to store arguments for auto duplication removal
    info <- func.info.map[[entry$'func']];
    if (is.null(info)) {
      info <- list('type'=entry$'type', 'argvs'=new.env());
      func.info.map[[entry$'func']] <- info;
    }
    info$'argvs'[[entry$'argv']] <- entry$'retn';
  }
  func.info.map;
}

gen.func <- function(func, type, argv, retn) {
  # check to see if it is bad arguments
  parseAndCheck <- function(what) {
    tryCatch({eval(parse(text=what))}, warning=print, error=function(e){valid<<-FALSE;});
  }
  mk.err <- function(msg) { list(type="err", msg=msg); }
  mk.src <- function(msg) { list(type="src", msg=msg); }
  valid <- TRUE;
  argv.obj <- parseAndCheck(argv);
  valid.argv <- valid;
  valid <- TRUE;
  retn.obj <- parseAndCheck(retn);
  valid.retn <- valid;
  # proper argument should always be packed in a list
  if (!valid.argv || !valid.retn) { 
    return (mk.err(paste("func:", func, "\ntype:", type, "\nargv:", argv,"\nretn:", retn,"\n")));
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

report <- function(gen.stat.map) {
  captured.builtin.total <- length(ls(gen.stat.map));
  builtin.total <- length(ls(baseenv()));
  summary.df <- data.frame(stringsAsFactors=FALSE);
  for (func in ls(gen.stat.map)) {
    summary.df <- rbind(summary.df, gen.stat.map[[func]]);
  }
  summary.df["Total" ,] <- colSums(summary.df);
  names(summary.df) <- c("TstGen", "BadArg");
  cat("========--------   Statistics   --------========\n");
  cat("\n");
  cat("* builtin captured:   ", captured.builtin.total, " / ", builtin.total, "\n", sep="");
  cat("\n");
  print(summary.df);
  cat("\n");
  cat("=================================================\n");

}

alter.arguments <- function(argv) {
## TODO  
}

