# operators <- c("(",":","%sep%","[","[[", "$","@", "<-", "<<-","=", "[<-","[[<-","$<-", "@<-", "+","-","*","/", "^","%%","%*%","%/%","<","<=","==","!=",">=",">","|","||","&","&&","!")
# operators.count <- 0
translateFastR <- function(root){
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=".[rR]$", recursive = TRUE, all.files = TRUE) 
    files <- Map(function (x) paste(root,"/",x, sep=""), files) 
  } else {
    files <- root
  }
  cache <- new.env();
  for (filename in files) {
    cat(filename, "\n")
    f.name <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
    f.name <- gsub("\\.", "", f.name)
    f.name <- gsub("<-", "assign", f.name)
    f.number <- cache[[f.name]]
    if (is.null(f.number))
      f.number <- 1
    if (f.name %in% operators){
      f.name <- "operators"
      f.number <- operators.count
      operators.count <- operators.count + 1
    }      
    sink(paste("tests/Testrgen", f.name, ".java", sep=""), append=TRUE)
    cat("\n\t@Test\n\tpublic void test", f.name, f.number, "(){", "\n", sep="")
    cat("\t\t")
    source(filename, local = FALSE)
    cat("\t}\n")
    sink()
    cache[[f.name]] <- f.number + 1
  }
}

# unlink("tests", TRUE)
# dir.create("tests")
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)

tests <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
#   code <- expression(code)
  code <- sapply(deparse((substitute(code))), trim)
  res <- ""
  for (line in code){
    if (!grepl("^\\{", line) && !grepl("^\\}", line)){
      if (grepl("^argv <-", line)){
        eval(parse(text=line));
        args <- length(argv);
        if (args > 0){
#           argv <- deparse(argv)
          argv <- substr(line, 28, nchar(line) - 3)          
          #           argv <- gsub("argv <- eval(parse(text=\"(.*)\"", "\\1", line)
          argv <- gsub('"',"'", argv)
          argv <- paste(argv, collapse="", sep="")
          res <- paste('argv <- ',argv, ';', sep="");
        }
        
      } else
      if (grepl("^do.call\\(", line)){
        name <- gsub("do.call\\((.*),(.*))", "\\1", line)
        res <- paste(res, name, "(", sep = "")
        if (args > 0) { 
          for (idx in 1:args) { 
            res <- paste(res, "argv[[",idx,"]],", sep=""); 
          } 
          res <- gsub(",$", ");", res)
        } else {
          res <- paste(res, ");")
        }
      } else {
        res <- paste(res, line)
      }
    }
  }
  res <- paste("assertEval(\"", res, "\");\n", sep="")
#   res <- gsub("'", "\\\\'", res)
  cat(res)
}

OrginizeTests <- function(root){
  files <- list.files(root, pattern=".java$", recursive = TRUE, all.files = TRUE) 
  for (filename in files){
    f.conn <- file(paste(root, filename, sep="/"), 'r+') 
    lines <- readLines(f.conn) 
    class.name <- gsub("(.*).java", "\\1", filename)
    writeLines(c("/*",  
                 " * This material is distributed under the GNU General Public License",
                 " * Version 2. You may review the terms of this license at", 
                 " * http://www.gnu.org/licenses/gpl-2.0.html",
                 " * ",
                 " * Copyright (c) 2014, Purdue University",  
                 " * Copyright (c) 2014, Oracle and/or its affiliates",
                 " * All rights reserved.", 
                 " */",
                 "package com.oracle.truffle.r.test.testrgen;\n", 
                 "import org.junit.*;\n",
                 "import com.oracle.truffle.r.test.*;\n",
                 paste("public class ", class.name, " extends TestBase {", sep=""),
                 lines,
                 "}"), 
               con = f.conn) 
    close(f.conn) 
  }
}

OrginizeTestCases <- function(root, res.dir){
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=".[rR]$", recursive = TRUE, all.files = TRUE) 
    files <- Map(function (x) paste(root,"/",x, sep=""), files) 
  } else {
    files <- root
  }
  if (!file.exists(res.dir))
    dir.create(res.dir)
  cache <- new.env();
  for (filename in files) {
    cat(filename, "\n")
    f.name <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
    f.name <- gsub("\\.", "", f.name)
    f.name <- gsub("<-", "assign", f.name)
    if (f.name %in% operators){
      f.name <- "operators"
    }      
    f.number <- cache[[f.name]]
    if (is.null(f.number)){
      f.number <- 1
      dir.create(paste(res.dir, f.name, sep="/"))      
    }
    res.file.name <- paste(res.dir, f.name, paste("tc_", f.name, "_", f.number, ".R", sep=""), sep="/")
    lines <- readLines(filename);
    res.conn <- file(res.file.name, "w")
    write(lines, res.conn, append=TRUE)
    close(res.conn)
    cache[[f.name]] <- f.number + 1
  }
}