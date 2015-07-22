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
    }
    res
}

#' @title Run R regression tests
#' 
#' Run test that are available with make test-Reg
#' @param exec_dir directory where command will be executed
#' @export
reg_tests <-  function(exec_dir = tempdir()) {
    test_dir <- normalizePath(switch(basename(getwd()),
                                     "tests" = "testhat/reg-tests",
                                     "testr" = "tests/testthat/reg-tests",
                                     "testthat" = "reg-tests"))
    invisible(runner(test_dir, exec_dir))
}

#' @title Run all R tests other than regression tests
#' 
#' Run test that are available with make test-all-devel
#' @param exec_dir directory where command will be executed
#' @export
all_tests <-  function(exec_dir = tempdir()) {
    test_dir <- normalizePath(switch(basename(getwd()),
                                     "tests" = "testhat/r-tests",
                                     "testr" = "tests/testthat/r-tests",
                                     "testthat" = "r-tests"))
    invisible(runner(test_dir, exec_dir))
}

#' @title Run R test from a folder
#' 
#' This function runs R script located in folder and gets results
#' @param test_dir directory with R scripts
#' @param exec_dir directory where command will be executed
#'
runner <- function(test_dir, exec_dir = tempdir()) {
    unlink(exec_dir, recursive = TRUE, force = TRUE)
    dir.create(exec_dir)
    files <- list.files(test_dir, pattern = "*.[rR]$", full.names = TRUE)
    files <- sapply(files, normalizePath)
    old_wd <- setwd(exec_dir)
    on.exit(setwd(old_wd))
    sapply(files, rcmd, env_vars = list("SRCDIR"=test_dir))
}
