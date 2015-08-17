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
                                     "tests" = "reg-tests",
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
                                     "tests" = "r-tests",
                                     "testr" = "inst/tests/r-tests",
                                     "testthat" = "r-tests"))
    invisible(runner(test_dir, file_patterns, exec_dir, verbose = verbose))
}

#' @title Run R test from a folder
#' 
#' This function runs R script located in folder and gets results
#' @param test_dir directory with R scripts
#' @param exec_dir directory where command will be executed
#' @param verbose if to print additional information (default \code{TRUE})
#'
runner <- function(test_dir, file_patterns = ".*",
                   exec_dir = tempdir(), rprofile, verbose = testr_options("verbose")) {
    unlink(exec_dir, recursive = TRUE, force = TRUE)
    dir.create(exec_dir)
    files <- list.files(test_dir, pattern = "\\.[rR]$", full.names = TRUE)
    ind <- t(sapply(file_patterns, function(x) grepl(paste(".*", x, ".*", sep=""), files)))
    ind <- sapply(1:ncol(ind), function(i) any(ind[,i]))
    files <- files[ind]
    files <- sapply(files, normalizePath)
    old_wd <- setwd(exec_dir)
    writeLines(testr_options("rprofile"), ".Rprofile")
    on.exit(setwd(old_wd))
    res <- parallel::mclapply(files, rcmd, env_vars = list("SRCDIR"=test_dir), verbose = verbose, mc.cores = 8)
    if (any(res == 1)) {
        failed <- which(res == 1)
        lapply(names(failed), function(x) { 
            cat("===File -", x, "\n")
            print(tail(readLines(paste(basename(x), "out", sep=""))))
        })
    }
    res
}

rprofile_check_5 <- '
.First <- function() {
library(testr)
library(utils)
bl <- testr:::blacklist
builtin_capture()
i <- %s
for (j in ((i - 1) * 5 + 1):(i * 5))
    decorate(bl[j])
}'

rprofile_check_single <- '
.First <- function() {
library(testr)
library(utils)
bl <- testr:::blacklist
builtin_capture()
i <- %s
decorate(bl[i])
}'

bl_filter <- function() {
    out_folder <- "/Users/romantsegelskyi/Programming/rstats/testr/blacklist_filter"
    unlink(out_folder, recursive = TRUE, force = TRUE)
    log_path <- file.path("logs")
    unlink(log_path, recursive = TRUE, force = TRUE)
    dir.create(out_folder)
    dir.create(log_path)
    for (i in 1:ceil(length(blacklist) / 5)) {
        file.remove(list.files(out_folder, full.names = TRUE, pattern = "*\\.Rout"))
        print(testr:::blacklist[((i - 1) * 5 + 1):(i * 5)])
        rprofile <- sprintf(rprofile_check_5, i)
        testr_options("rprofile", rprofile)
        res <- reg_tests(out_folder)
        if (any(res == 1)) {
            cat(sprintf("===for %s failed\n", i))
            cat("===Running separately for each function\n")
            for (j in ((i - 1) * 5 + 1):(i * 5)) {
                file.remove(list.files(out_folder, full.names = TRUE, pattern = "*\\.Rout"))
                rprofile <- sprintf(rprofile_check_single, j)
                testr_options("rprofile", rprofile)
                cat(sprintf("\t+++Trying %s - ", blacklist[j]))
                res <- reg_tests("blacklist_filter")
                if (any(res == 1)) {
                    cat("failed\n")
                    errored_out <- paste(names(which(res == 1)), "out", sep="")
                    dir.create(sprintf("%s/log_%s", log_path, blacklist[j]), recursive = TRUE)
                    sapply(errored_out,
                           function(x) file.rename(x, sprintf("%s/log_%s/%s", log_path, blacklist[j], basename(x))))
                } else {
                    cat("successful\n")
                }
            }
        }
    }
}
