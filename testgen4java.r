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
testgen <- function(output.dir, ifile, verbose=FALSE) {

  if (verbose) {
    cat("Output:", output.dir, "\n");
    cat("Ifile:", ifile, "\n");
  }
  
  if (missing(output.dir)) stop("A output directory must be provided!");
  if (!file.exists(ifile)) stop("Intermediate file doesn't exist!");
  bad.argv.file <- file.path(output.dir, "bad_arguments", fsep=.Platform$file.sep);
  if (!file.create(bad.argv.file)) stop("Unable to create file: ", bad.argv.file);
  
  lines <- readLines(ifile);
  cache <- new.env();

  for(i in seq(1,length(lines),4)) {
    func <- lines[i+0];
    type <- lines[i+1];
    args <- lines[i+2];
    retn <- lines[i+3];
    tc.file <- ensure.tc.file(output.dir, func, cache);
    feedback <- gen.func(func, type, args, retn);
#### see what we get
    if (feedback$'type' == "err") {
#### the captured information is not usable
      write(feedback$'msg', file=bad.argv.file, append=TRUE);
    } else if (feedback$'type' == "src") {
#### good, we get the source code
      write(feedback$'msg', file=tc.file, append=TRUE);
    } else {
      stop("Not reached!");
    }
  }
  for(f in ls(cache)) {
    close(cache[[f]]);
  }
}

ensure.tc.file <- function(path, name, cache) {
  tc.file <- cache[[name]];
  if (is.null(tc.file)) {
    name <- gsub(.Platform$file.sep, "sep", name);
    tc.file <- file.path(path, paste("tc_", name, ".r", sep=""), fsep=.Platform$file.sep);
    if (!file.exists(tc.file) && !file.create(tc.file)) stop("Unable to create file: ", tc.file);
  }
  return(tc.file);
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
    src <- paste(src, "do.call(`", func, "`, argv);", sep="");
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
      src <- paste(src, ".Internal(`", func, "`());", sep="");
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


args <- commandArgs(trailingOnly=TRUE);
testgen(args[1], args[2], TRUE);
