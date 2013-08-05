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

library("testthat")

context("target")

target <- function(name, path, version, ..., args = NULL, clazz = NULL) {
    name <- substitute(name)
    if (typeof(name) != "character")
        name <- as.character(name)
    result <- list(
            name = name,
            path = path,
            args = args,
            version = version
        )
    fields <- list(...)
    if (length(fields) > 0) {
        if (length(names(fields)) != length(fields))
            stop("All fields to target must be named")
        for (fn in names(fields))
            if (fn %in% c("name", "path", "args", "version", ""))
                stop("Unable to add reserved or empty target field ", fn)
        result <- c(result, fields)
    }
    class(result) <-  c(clazz, "target")
    result
}

test_that("target with minimal arguments", {
    t <- target(tName, "path", 1.0)
    expect_equal(t$name, "tName")
    expect_equal(t$path, "path")
    expect_true(is.null(t$defaultArgs))
    expect_equal(t$version, 1.0)
})

test_that("reserved or unnamed fields cannot be added to the target", {
    expect_error({
        target(t, "p", 1.0, 5, 6)
    })
    expect_error({
        target(t, "p", 1.0, version = 2)
    })
})

test_that("fields can be added to the target", {
    t <- target(tName, "path", 1.0, arch = "x86")
    expect_equal(t$name, "tName")
    expect_equal(t$path, "path")
    expect_true(is.null(t$defaultArgs))
    expect_equal(t$version, 1.0)
    expect_equal(t$arch, "x86")
})


charAt <- function(s, index) {
    substr(s, index, index)
}


getProgramOutput.target <- function(target, testResult) {
    o <- testResult$cout
    result <- NULL
    valid <- FALSE
    for (line in o) {
        l <- charAt(line, 1)
        if (valid) {
            if (!l %in% c(">", "+")) {
                result <- c(result, line)
            }
        } else {
            if (identical(l, ">"))
                valid <- TRUE
        }
    }
    result    
}

getProgramOutput <- function(target, ...) {
    UseMethod("getProgramOutput",target)
}

#' Implements only the default adapter. 
#' 
#' Default adapter works like the default adapter in Python for backwards compatibility. 
getAdapterResult.target <- function(target, testresult, adapter = "default") {
    if (adapter != "default")
        return (NULL)
    
}


getAdapterResult <- function(target, ...) {
    UseMethod("getAdapterResult", target)
}

execute.target <- function(target, test) {
    r <- command(target$path, target$args, input = test$code, separateOutErr = TRUE)
    print(getProgramOutput(target, r))
    r$targetOutput <- getProgramOutput(target, r)
    r$target <- target
    r
}

execute <- function(target, ...) {
    UseMethod("execute",target)
}

#' Creates a windows gnu-r target. 
#' TODO path must be compulsory
createTarget.gnur_win <- function(path = NULL) {
    # todo get rid of this
    if (missing(path))
        path <- "c:\\Program Files\\R\\R-3.0.1\\bin\\R.exe"
    output <- command(path,"--version")
    version <- strsplit(output$cout[[1]], " ")[[1]][[3]]
    arch <- strsplit(output$cout[[3]], " ")[[1]][[2]]
    target(
        name = "gnur-win",
        path = path, 
        version = version,
        args = "--vanilla",
        clazz = "gnur",
        arch = arch
    )
}



print.target <- function(target) {
    cat("Target class:  ", class(target), "\n")
    for (n in names(target)) {
        cat(n,":", sep = "")
        for (i in 1: (15 - nchar(n)))
            cat(" ", sep = "")
        cat(target[[n]],"\n")
    }
}

#gnur <- target("gnur",RPATH,"--vanilla", 1.0, clazz="gnur")



