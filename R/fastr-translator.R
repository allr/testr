copyright_header <-
"\\/*
* This material is distributed under the GNU General Public License
* Version 2. You may review the terms of this license at
* http://www.gnu.org/licenses/gpl-2.0.html
*
* Copyright (c) 2014, Purdue University
* Copyright (c) 2014, Oracle and/or its affiliates
* All rights reserved.
*/
package com.oracle.truffle.r.test.testrgen;

import org.junit.*;
import com.oracle.truffle.r.test.*;"

#' @title Translate Test Cases to FastR style
#' 
#' This function is respinsible generating Java testcases for FastR. 
#' If there is a folder with already existing test cases translated from TestR to FastR, then the function will append test cases to proper files in that folder.
#' Uses unility function for extraction of test case information. Idea is that to instead of running a test when calling test function, it collects test information and converts it to FastR test
#' @param r.test.root folder with R testcases
#' @param fastr_test_folder folder with FastR test cases in Java
#' @export
#'
fastr_translate <- function(rtest_folder, fastr_test_folder = "tests/"){
    # unility function for extraction of test case information. Replaces o
    fastr_test <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
        # extract statements from code and replace double quotes with single ones
        str <- as.list(substitute(code))[-1]
        str <- lapply(str, function(x) paste(deparse(x), collapse = ""))
        str <- gsub('"', "\\\\'", str)
        str <- paste(str, collapse = ';"+\n\t\t\t"', sep="")
        res <- sprintf("\t\tassertEval(\"%s\");\n", str)
        res
    }
    fastr_test_files <- vector()
    # test.folder sanity check
    if (file.exists(fastr_test_folder)){
        if (!file.info(fastr_test_folder)$isdir) stop("Specified location of tests is not a folder")
        fastr_test_files <- get_all_files(fastr_test_folder, pattern = ".java$", full.names = F)
        fastr_test_files <- sapply(fastr_test_files, function(x) gsub("TestrGenBuiltin(.*).java", "\\1", x))
    } else {
        dir.create(fastr_test_folder)
    }

    r_test_files <- get_all_files(rtest_folder)
    # cache for storing information about functions (code and test case count)
    function.cache <- list()

    for (filename in r_test_files) {
        function.name <- extract_func_name(filename)
        function_cache_entry <- function.cache[[function.name]]

        if (is.null(function_cache_entry)) {
            function_cache_entry <- list()
            if (function.name %in% fastr_test_files) {
                fastr_test_file <- sprintf("%s/TestrGenBuiltin%s.java", fastr_test_folder, function.name)
                fastr_test_code <- readLines(fastr_test_file)
                fastr_test_code <- fastr_test_code[1:(length(fastr_test_code) - 1)]

                function_cache_entry$number <- length(grep("@Test", fastr_test_code)) + 2
                function_cache_entry$code <- fastr_test_code
            } else {
                function_cache_entry$number <- 1
                function_cache_entry$code <- c(copyright_header, sprintf("// Checkstyle: stop line length check\n
                                                                 public class TestrGenBuiltin%s extends TestBase {",
                                                                         function.name))
            }
        }
        # for operators unify under same file
        if (function.name %in% operators){
            function_cache_entry <- function.cache[["operators"]]
            function.name <- "operators"
        }

        # evaluate R test file in special environment with replaces test function
        temp.env <- new.env();
        temp.env$test <- fastr_test
        with(temp.env, source(filename, local = TRUE))
        # create Java testcase
        test.code <- sprintf("\n\t@Test\n\tpublic void test%s%d() {\n%s\t}\n",
                             function.name,
                             function_cache_entry$number,
                             temp.env$res)
        # save information back to cache
        function.cache[[function.name]]$number <- function_cache_entry$number + 1
        function.cache[[function.name]]$code <- c(function_cache_entry$code, test.code)
    }
    # generate write down information to Java testcase files
    for (function.name in names(function.cache)){
        entry <- function.cache[[function.name]]
        file.name <- paste(fastr_test_folder, "/TestrGenBuiltin", function.name, ".java", sep="") #nolint
        writeLines(c(entry$code, "}"), file.name)
    }
}

#' @title Function that tags failing test cases for FastR as ignored
#' 
#' This function tags test cases with Ignore preporcessor. It requires a file with results of running tests
#' @param fastr_test_root folder with FastR test cases
#' @param result.file file with result of running FastR test cases
#' @export
#'
fastr_tag_ignored <- function(fastr_test_root, result.file) {
    lines <- readLines(result.file)
    tests <- vector()
    for (line in lines)
        if (grepl("Micro-test failure", line))
            tests <- c(tests, gsub("Micro-test failure: (.*)\\((.*)\\)", "\\1", line))

    trim <- function (x) gsub("^\\s+|\\s+$", "", x)

    files <- get_all_files(fastr_test_root, pattern = "*.java", TRUE)
    for (tc_file in files){
        lines <- readLines(tc_file)
        sink(tc_file)
        for (i in 1:length(lines)){
            line <- lines[i]
            cat(line, "\n", sep="")
            if (i < length(lines) && grepl("@Test", line)){
                tc_name <- gsub("public void (.*)\\((.*)", "\\1", trim(lines[i + 1]))
                if (tc_name %in% tests)
                    cat("    @Ignore\n")
            }
        }
        cat("\n")
        sink()
    }
}
