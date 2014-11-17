GetAllFiles <- function(root, pattern = ".[rR]$", full.names = T){
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=pattern, recursive = TRUE, all.files = TRUE, full.names = full.names) 
  } else {
    files <- root
  }
  files
}

ExtractFunctionName <- function(filename, modify.characters = TRUE){
  function.name <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
  if (modify.characters){
    function.name <- gsub("\\.", "", function.name)
    function.name <- gsub("<-", "assign_", function.name)
    function.name <- gsub("\\[", "extract_parentasis_", function.name)
    function.name <- gsub("\\$", "extract_dollar_", function.name)
    function.name <- gsub("\\+", "plus_", function.name)
    function.name <- gsub("\\-", "minus_", function.name)
    function.name <- gsub("&", "and_", function.name)
    function.name <- gsub("\\*", "times_", function.name)

  }
  function.name
}

copyright.header <- c("/*",  
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
                      "import com.oracle.truffle.r.test.*;\n")

#' @title Translate Test Cases to FastR style
#' 
#' This function is respinsible generating Java testcases for FastR. 
#' If there is a folder with already existing test cases translated from TestR to FastR, then the function will append test cases to proper files in that folder
#' @param r.test.root folder with R testcases
#' @param fastr.test.folder folder with FastR test cases in Java
#' @export
#'
TranslateFastr <- function(r.test.folder, fastr.test.folder = "tests/"){
  fastr.test.files <- vector()
  # test.folder sanity check
  if (file.exists(fastr.test.folder)){
    if (!file.info(fastr.test.folder)$isdir) stop("Specified location of tests is not a folder")
    fastr.test.files <- GetAllFiles(fastr.test.folder, pattern = ".java$", full.names = F)
    fastr.test.files <- sapply(fastr.test.files, function(x) gsub("TestrGenBuiltin(.*).java", "\\1", x))
  } else {
    dir.create(fastr.test.folder)
  }
  
  r.test.files <- GetAllFiles(r.test.folder)
  # cache for storing information about functions (code and test case count)
  function.cache <- list()
  
  for (filename in r.test.files) {
    function.name <- ExtractFunctionName(filename)
    function.cache.entry <- function.cache[[function.name]]

    if (is.null(function.cache.entry)) {
      function.cache.entry <- list()
      if (function.name %in% fastr.test.files) {
        fastr.test.file <- sprintf("%s/TestrGenBuiltin%s.java", fastr.test.folder, function.name)
        fastr.test.code <- readLines(fastr.test.file)
        fastr.test.code <- fastr.test.code[1:(length(fastr.test.code) - 1)]
        
        function.cache.entry$number <- length(grep("@Test", fastr.test.code)) + 2
        function.cache.entry$code <- fastr.test.code
      } else {
        function.cache.entry$number <- 1
        function.cache.entry$code <- c(copyright.header, sprintf("public class TestrGenBuiltin%s extends TestBase {", function.name))
      }
    }
    # for operators unify under same file
    if (function.name %in% operators){
      function.cache.entry <- function.cache[["operators"]]
      function.name <- "operators"
    }
    
    # evaluate R test file in special environment with replaces test function
    temp.env <- new.env();
    temp.env$test <- TestFastr
    with(temp.env, res <- source(filename, local = TRUE)$value)
    # create Java testcase
    test.code <- sprintf("\n\t@Test\n\tpublic void test%s%d() {\n%s\t}\n", function.name, function.cache.entry$number, temp.env$res)
    # save information back to cache
    function.cache[[function.name]]$number <- function.cache.entry$number + 1
    function.cache[[function.name]]$code <- c(function.cache.entry$code, test.code)
  }
  # generate write down information to Java testcase files
  for (function.name in names(function.cache)){
    entry <- function.cache[[function.name]]
    file.name <- paste(fastr.test.folder, "/TestrGenBuiltin", function.name, ".java", sep="")
    writeLines(c(entry$code, "}"), file.name)
  }
}

TestFastr <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
  str <- deparse(as.list(substitute(code)[-1])) # extract statements from code and replace double quotes with single ones   
  str <- gsub('"', "\\\\'", str)
  str <- paste(str, collapse = '"+\n\t\t\t"', sep="")         
  res <- sprintf("\t\tassertEval(\"%s\");\n", str)  
  res 
}