# operators <- c("(",":","%sep%","[","[[", "$","@", "<-", "<<-","=", "[<-","[[<-","$<-", "@<-", "+","-","*","/", "^","%%","%*%","%/%","<","<=","==","!=",">=",">","|","||","&","&&","!")
# operators.count <- 0
# gsub("(.*)argv <- (.*)do.call(.*)", "\\2", c)

get.all.files <- function(root, pattern = ".[rR]$", full.names = T){
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=pattern, recursive = TRUE, all.files = TRUE, full.names = full.names) 
  } else {
    files <- root
  }
  files
}

translateFastR <- function(root, test.folder = "tests/"){
  files <- get.all.files(root)
  test.files <- vector()
  if (file.exists(test.folder)){
    if (!file.info(test.folder)$isdir) stop("Specified location of tests is not a folder")
    test.files <- get.all.files(test.folder, pattern = ".java$", full.names = F)
    test.files <- sapply(test.files, function(x) gsub("Testrgen(.*).java", "\\1", x))
  } else {
    dir.create(test.folder)
  }
  cache <- new.env()
  for (filename in files) {
    cat(filename, "\n")
    f.name <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
    f.name <- gsub("\\.", "", f.name)
    f.name <- gsub("<-", "_assign_", f.name)
    f.name <- gsub("\\[", "_extract1_", f.name)
    f.name <- gsub("\\$", "_extract2_", f.name)
    f.number <- cache[[f.name]]
    if (is.null(f.number)) {
      if (f.name %in% test.files) {
        file.name <- sprintf("%s/Testrgen%s.java", test.folder, f.name)
        test.file <- readLines(file.name)
        test.file <- test.file[1:(length(test.file) - 1)]
        writeLines(test.file, file.name)
        f.number <- length(grep("public void", test.file)) + 1
      } else {
        f.number <- 1
      }
    }
    if (f.name %in% operators){
      f.name <- "operators"
      f.number <- operators.count
      operators.count <- operators.count + 1
    }      
    sink(paste(test.folder, "/Testrgen", f.name, ".java", sep=""), append=TRUE)
    cat("\n\t@Test\n\tpublic void test", f.name, f.number, "() {", "\n", sep="")
    cat("\t\t")
    temp.env <- new.env();
    temp.env$test <- test.fastr
    with(temp.env, source(filename, local = TRUE))
    cat("\t}\n")
    sink()
    cache[[f.name]] <- f.number + 1
  }
}

# unlink("tests", TRUE)
# dir.create("tests")


test.fastr <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
  str <- gsub('"', "'", as.list(substitute(code)[-1]))   
  str <- dQuote(paste(str, collapse = "\n\t\t            "))         
  res <- sprintf("assertEval(%s);\n", str)
  cat(res)  
}

OrginizeTests <- function(root){
  files <- list.files(root, pattern=".java$", recursive = TRUE, all.files = TRUE) 
  for (filename in files){
    f.conn <- file(paste(root, filename, sep="/"), 'r+') 
    lines <- readLines(f.conn) 
    class.name <- gsub("(.*).java", "\\1", filename)
    if (!grepl("This material", lines)){
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
    } else {
      cat("}\n", con = f.conn)
    }
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