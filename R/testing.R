#' @title Run R script 
#' 
#' This function runs R script through R CMD BATCH
#' @param file R script to run
#' @param env_vars environment variables
#' @param verbose if to print additional information (default \code{TRUE})
#'
rcmd <- function(file, env_vars = NULL, verbose = testr_options("verbose")) {
    if (verbose) {
        cat("File - ", file, "\n")
    }
    if (!file.exists(file))
        stop("File doesn't exist!")
    env_values <- sapply(names(env_vars), Sys.getenv)
    do.call(Sys.setenv, env_vars)
    cmd <- sprintf("R CMD BATCH --no-save --no-restore '%s'", file)
    res <- system(cmd)
    do.call(Sys.setenv, as.list(env_values))
    if (res != 0 && verbose) {
        message(paste("File", basename(file), "failed"))
        print(tail(readLines(paste(getwd(), "/", basename(file), "out", sep="")), 10))
    }
    res
}

#' @title Run R regression tests
#' 
#' Run test that are available with make test-Reg
#' @param exec_dir directory where command will be executed
#' @param verbose if to print additional information (default \code{TRUE})
#' @export
reg_tests <-  function(exec_dir = tempdir(),
                       file_patterns = ".*", verbose = testr_options("verbose")) {
    test_dir <- normalizePath(switch(basename(getwd()),
                                     "tests" = "testhat/reg-tests",
                                     "testr" = "inst/tests/reg-tests",
                                     "testthat" = "reg-tests"))
    invisible(runner(test_dir, file_patterns, exec_dir, verbose = verbose))
}

#' @title Run all R tests other than regression tests
#' 
#' Run test that are available with make test-all-devel
#' @param exec_dir directory where command will be executed
#' @param verbose if to print additional information (default \code{TRUE})
#' @export
all_tests <-  function(exec_dir = tempdir(),
                       file_patterns = ".*", verbose = testr_options("verbose")) {
    test_dir <- normalizePath(switch(basename(getwd()),
                                     "tests" = "testhat/r-tests",
                                     "testr" = "inst/tests/r-tests",
                                     "testthat" = "r-tests"))
    invisible(runner(test_dir, file_patterns, exec_dir, verbose = verbose))
}

rprofile_capture <- '
.First <- function() {
    library(testr)
    library(utils)
    builtin_capture()
}
'

#' @title Run R test from a folder
#' 
#' This function runs R script located in folder and gets results
#' @param test_dir directory with R scripts
#' @param exec_dir directory where command will be executed
#' @param verbose if to print additional information (default \code{TRUE})
#'
runner <- function(test_dir, file_patterns = ".*",
                   exec_dir = tempdir(), verbose = testr_options("verbose")) {
    unlink(exec_dir, recursive = TRUE, force = TRUE)
    dir.create(exec_dir)
    files <- list.files(test_dir, pattern = "\\.[rR]$", full.names = TRUE)
    ind <- t(sapply(file_patterns, function(x) grepl(paste(".*", x, ".*", sep=""), files)))
    ind <- sapply(1:ncol(ind), function(i) any(ind[,i]))
    files <- files[ind]
    files <- sapply(files, normalizePath)
    old_wd <- setwd(exec_dir)
    writeLines(rprofile_capture, ".Rprofile")
    on.exit(setwd(old_wd))
    sapply(files, rcmd, env_vars = list("SRCDIR"=test_dir), verbose = verbose)
}
