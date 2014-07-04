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
  if (!file.exists(bad.argv.file) && !file.create(bad.argv.file)) stop("Unable to create file: ", bad.argv.file);
  
  lines <- readLines(ifile);
  cache <- new.env();
  i <- 1
  while (i < length(lines)){
    k_sym <- 1
    k_value <- 1
    symb <- vector()
    vsym <- vector()
    symb[k_sym] <- ""
    vsym[k_value] <- ""
    while (substr(lines[i], 1, 4) == "symb"){
      symb[k_sym] <- paste(symb[k_sym], deparse.line(lines[i]), sep = "") 
      i <- i + 1
      k_sym <- k_sym + 1
      symb[k_sym] <- ""
      while (substr(lines[i], 1, 4) == "vsym"){
        vsym[k_value] <- paste(vsym[k_value], 
                               substr(lines[i], 6, nchar(lines[i])),
                               "\n",
                               sep="")
        i <- i + 1
      }
      k_value <- k_value + 1
      vsym[k_value] <- ""
    }
    length(symb) <- length(symb) - 1
    length(vsym) <- length(vsym) - 1
    if (length(symb) != length(vsym))  # Sanity checks remove later
      stop("Something is seriously wrong. Value or symbols")
    if (substr(lines[i], 1, 4) != "func")
      stop("Something is seriously wrong. Func")
    
    func <- deparse.line(lines[i])
    i <- i + 1
    body <- ""
    while (substr(lines[i], 1, 4) == "body"){
      body <- paste(body, 
                    substr(lines[i], 6, nchar(lines[i])),
                    "\n",
                    sep="")
      i <- i + 1
    }
    args <- ""
    while (substr(lines[i], 1, 4) == "args"){
      args <- paste(args, 
                    substr(lines[i], 6, nchar(lines[i])),
                    sep="")
#      args <- paste(args, "\n")
      i <- i + 1
    }
    retn <- ""
    while (substr(lines[i], 1, 4) == "retn"){
      retn <- paste(retn, 
                    substr(lines[i], 6, nchar(lines[i])),
                    sep="")
#      retn <- paste(retn, "\n")
      i <- i + 1
    }
    i <- i + 1
    tc.file <- ensure.tc.file(output.dir, func, cache);
    feedback <- gen.func(symb, vsym, func, body, args, retn); # added symb and vsym
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

# Like true false
deparse.line <- function(l){
  if (grepl("quote\\(", l)){
    ret.line <- strsplit(l, "\\(")[[1]][2];
    if (substr(ret.line, nchar(ret.line), nchar(ret.line)) == ")")
      ret.line <- substr(ret.line, 0, nchar(ret.line) - 1)
  }else{
    ret.line <- substr(l, 6, nchar(l));     
  }
  ret.line
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

gen.func <- function(symb, vsym, func, body, argv, retn) {
  # check to see if it is bad arguments
  parseAndCheck <- function(what) {
    tryCatch({eval(parse(text=what))}, warning=print, error=function(e){valid<<-FALSE;});
  }
  mk.err <- function(msg) { list(type="err", msg=msg); }
  mk.src <- function(msg) { list(type="src", msg=msg); }
  # check validity of symbol values
  # we need additional vector
  # using vector because need to keep track of indexes
  temp.valid.vsym <- vector();
  vsym.obj <- vector();
  valid <- TRUE;
  invalid.symbols <- vector();
  if (length(symb) > 0 && symb[1] != ""){
    for (i in 1:length(vsym)){
#      vsym.obj[i] <- parseAndCheck(vsym[i]);
      parseAndCheck(vsym[i]);
      if (!valid){
        invalid.symbols <- append(invalid.symbols, i)  
      }
    }
  }
  valid.vsym <- all(temp.valid.vsym);
  # check validity of arguments
  valid <- TRUE;
  argv.obj <- parseAndCheck(argv);
  valid.argv <- valid;
  # check validity of return values
  valid <- TRUE;
  retn.obj <- parseAndCheck(retn);
  valid.retn <- valid;

  # proper argument should always be packed in a list
  if (!valid.argv || !valid.retn) {
    return (mk.err(paste("func:", func, "\nbody:", body, "\nargv:", argv,"\nretn:", retn,"\n")));
  }  
  # TODO: potentially good arguments, alter it 
  #  argv.obj.lst <- alter.arguments(argv.obj);

  # get variables and values
  var <- ""
  if (length(symb) > 0 && symb[1] != "" && valid.vsym){
    for (i in 1:length(symb)){
      var <- paste(var, symb[i], "<-", vsym[i], "\n", sep=" ")
    }
  }
  # other stuff
  args <- length(argv.obj);
  f.search <- utils::getAnywhere(func)
  if (is.list(f.search$objs) & length(f.search$objs) == 0){
    call <- paste(func, "<-", body, "\n", sep="");
  }else{
    call <- paste(func, "<-",  "utils::getAnywhere(",func,")[1]",";\n", sep="")
  }
  if (args > 0) {
    call <- paste(call, "argv <- eval(parse(text=", deparse(argv), "));", "\n", sep="");
  } else {
    call <- paste(call, "argv <- list();", "\n", sep="");
  }
  if (grepl("`", func)){
    call <- paste(call, "do.call(", func, ", argv);", sep="");
  }else{ 
    call <- paste(call, "do.call('", func, "', argv);", sep="");
  }
  if (length(symb) > 0 && symb[1] != "")
    call <- paste(var, call, sep=""); # added because of variables
  src <- ""
  src <- paste(src, "expected <- eval(parse(text=", deparse(retn), "));\n",  sep="");
  src <- paste(src, "test(id=0, code={\n", call, "\n}, o=expected);\n", sep="")
  list(type="src", msg=src);
}


#args <- commandArgs(trailingOnly=TRUE);
#testgen(args[1], args[2], TRUE);
#testgen("temp", "~/Waterloo/rWD/testr/gen/2014_04_04_18_26_32//1396656018523")