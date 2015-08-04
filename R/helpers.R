#' @title Get function name from filename
#' 
#' This function extracts a function name from testcase file name
#' @param filename filename
#' @seealso process_tc
get_function_name <- function(filename){
    name <- gsub("(.*)[:]+(.*)\\.[rR]$", "\\2", filename)
    if (grepl("_", name))
        name <- gsub("(.*)_(.*)", "\\1", name)
    name
}

#' @title Check if function is S3 generic
#' 
#' Determine if function has a call to UseMethod. In that case there is no need to capture it.
#' @param fname function name
#' @seealso Decorate
is_s3_generic <- function(fname, env=.GlobalEnv) {
    f <- get(fname, mode = "function", envir = env)
    if (is.null(body(f))) return(FALSE)
    uses <- codetools::findGlobals(f, merge = FALSE)$functions
    any(uses == "UseMethod")
}


#' @title Estimate the number of test cases
#'
#' Estimate the number of test cases in the give path
#' @param path path to check
#' @seealso process_tc
get_num_tc <- function(path){
    if (file.info(path)$isdir) {
        path <- list.files(path, pattern = "\\.[rR]$", recursive = TRUE, all.files = TRUE)
    }
    lines <- vector()
    for (file in path) {
        lines <- c(lines, readLines(file))
    }
    length(grep("test\\(id",lines))
}

#' @title Clean temporary directory
#'
#' Make sure temp dir is empty by deleting unnecessary files
clean_temp <- function() {
    for (file in list.files(cache$temp_dir, full.names = T, pattern = "\\.RData|\\.[rR]$")) {
        file.remove(file)
    }
}

parse_eval <- function(what) {
    tryCatch({
        eval(parse(text=what));
        TRUE
        },
        error=function(e) {
            FALSE
            })
}

#' @title Quote language from evaluation
#'
#' In certain cases, language arguments (like calls), need to be quoated
#' @param arg list of arguments
#' @seealso GenerateTC
quoter <- function(arg) {
    if (is.list(arg)) {
        org.attrs <- attributes(arg)
        res <- lapply(arg, function(x) if(is.language(x)) enquote(x) else quoter(x))
        attributes(res) <- org.attrs
        res
    }
    else arg
}

#' @title Removes prefixes and quote from line
#'
#' @description Used for processing capture file information. Deletes prefixes to get essential information
#' @param l input line
#' @seealso ProcessClosure
substr_line <- function(l){
    if (grepl("^quote\\(", l)){
        ret.line <- strsplit(l, "\\(")[[1]][2];
        if (substr(ret.line, nchar(ret.line), nchar(ret.line)) == ")")
            ret.line <- substr(ret.line, 0, nchar(ret.line) - 1)
    } else {
        ret.line <- substr(l, 7, nchar(l))
    }
    ret.line
}

#' @title Check if line starts with prefix
#'
#' @param prefix prefix
#' @param x text to be checked
#' @seealso GenerateTC
starts_with <- function(prefix, x) {
    grepl(paste("^", prefix, sep=""), x)
}

#' @title Find test directory for package
#'
#' @param path package path
#' @seealso CapturePackage
find_tests <- function(path) {
    testthat <- file.path(path, "tests", "testthat")
    if (file.exists(testthat) && file.info(testthat)$isdir) {
        return(testthat)
    }
    inst <- file.path(path, "inst", "tests")
    if (file.exists(inst) && file.info(inst)$isdir) {
        return(inst)
    }
    warning("No testthat directories found in ", path, call. = FALSE)
    return(NULL)
}

#' Reassing object in the namespace
#'
#' Record that particual line was executed. Used in statement coverage, needed for namespace replacement
#' @param name name of an object to be replaced
#' @param obj object that will be put in the environment
#' @param env environment to be replaced in
reassing_in_env <- function(name, obj, env) {
    if (exists(name, env)) {
        if (bindingIsLocked(name, env)) {
            unlockBinding(name, env)
            assign(name, obj, envir = env)
            lockBinding(name, env)
        } else {
            assign(name, obj, envir = env)
        }
    }
}

#' @title Get all files with specific pattern
#' 
#' This function is respinsible for leturning all files from specified folder
#' @param root input folder
#' @param pattern pattern of files to be searched for
#' @param full.names if full path to files should be returned
#'
get_all_files <- function(root, pattern = ".[rR]$", full.names = T){
    if (file.info(root)$isdir){
        files <- list.files(root, pattern=pattern, recursive = TRUE, all.files = TRUE, full.names = full.names)
    } else {
        files <- root
    }
    files
}

#' @title Get function name without special characters
#' 
#' This function is respinsible for extractng function name from test file name and removing special characters
#' @param filename filename to be processed
#' @param modify.characters if special characters should be removed
#'
extract_func_name <- function(filename, modify.characters = TRUE){
    fname <- filename
    if (grepl(".[rR]$", filename)) {
        fname <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
    }
    if (fname %in% operators) {
        fname <- "operators"
    }
    if (modify.characters){
        fname <- gsub("\\.", "", fname)
        fname <- gsub("<-", "assign_", fname)
        fname <- gsub("\\[", "extract_parentasis_", fname)
        fname <- gsub("\\$", "extract_dollar_", fname)
        fname <- gsub("\\+", "plus_", fname)
        fname <- gsub("\\-", "minus_", fname)
        fname <- gsub("&", "and_", fname)
        fname <- gsub("\\*", "times_", fname)
    }
    fname
}
