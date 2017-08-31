#' Check if function is S3 generic
#'
#' @description Determine if function has a call to UseMethod. In that case there is no need to capture it.
#' @param fname function name
#' @param env environment to check aganist. Default \code{.GlobalEnv}
#' @seealso Decorate
is_s3_generic <- function(fname, env=.GlobalEnv)
{
    f <- get(fname, mode = "function", envir = env)
    if (is.null(body(f))) return(FALSE)
    uses <- codetools::findGlobals(f, merge = FALSE)$functions
    any(uses == "UseMethod")
}

#' Clean temporary directory
#'
#' @description Make sure temp dir is empty by deleting unnecessary files
clean_temp <- function()
{
    for (file in list.files(cache$temp_dir, full.names = TRUE, pattern = "\\.RData|\\.[rR]$")) {
        file.remove(file)
    }
}

#' Parse and evaluate
#'
#' @description Function that wraps parse(eval(...)) call with tryCatch
#' @param what text to be parse and evaluate
parse_eval <- function(what)
{
    tryCatch({
        eval(parse(text=what))
        TRUE
        },
        error=function(e) {
            FALSE
            })
}

#' Quote language from evaluation
#'
#' @description In certain cases, language arguments (like calls), need to be quoated
#' @param arg list of arguments
quoter <- function(arg)
{
    if (is.list(arg)) {
        org.attrs <- attributes(arg)
        res <- lapply(arg, function(x) if(is.language(x)) enquote(x) else quoter(x))
        attributes(res) <- org.attrs
        res
    }
    else arg
}

#' Removes prefixes and quote from line
#'
#' @description Used for processing capture file information. Deletes prefixes to get essential information
#' @param l input line
#' @seealso ProcessClosure
substr_line <- function(l)
{
    if (grepl("^quote\\(", l)){
        ret.line <- strsplit(l, "\\(")[[1]][2];
        if (substr(ret.line, nchar(ret.line), nchar(ret.line)) == ")")
            ret.line <- substr(ret.line, 0, nchar(ret.line) - 1)
    } else {
        ret.line <- substr(l, 7, nchar(l))
    }
    ret.line
}

#' Check line's starting prefix
#' @description Check if line starts with prefix
#'
#' @param prefix prefix
#' @param x text to be checked
#' @seealso GenerateTC
starts_with <- function(prefix, x)
{
    grepl(paste("^", prefix, sep=""), x)
}

#' Find test directory for package
#'
#' @description Find a known test location for the package
#' @param path package path
#' @seealso CapturePackage
find_tests <- function(path)
{
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
#' @description Record that particual line was executed.
#' Used in statement coverage, needed for namespace replacement
#' @param name name of an object to be replaced
#' @param obj object that will be put in the environment
#' @param env environment to be replaced in
reassing_in_env <- function(name, obj, env)
{
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

#' Get function name without special characters
#'
#' @description This function is respinsible for extractng function name from test file name and removing special characters
#' @param filename filename to be processed
#' @param modify.characters if special characters should be removed
#'
extract_func_name <- function(filename, modify.characters = TRUE)
{
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

#' Parse function names from objects
#' @description  Parses given function names to a list of name, package characters.
#' If package is not specified, NA is returned instead of its name.
#'
#' @param ... Functions either as character vectors, or package:::function expressions.
#' @return List of parsed package and function names as characters.
parseFunctionNames <- function(...)
{
    args <- unlist(list(...))
    res <- list()
    getInfo <- function(vector, arg, special) {
        info <- c()
        if (length(vector) > 2) {
            warning( sprintf("Invalid function name: %s\n", arg ) )
            info <- c(NA, NA)
        } else if (length(vector) == 2) {
            info <- unlist( ifelse(nchar(vector[1]) == 0, list(c(NA, vector[2])), list(vector)) )
        } else {
            info <- unlist( ifelse(nchar(vector) == 0, list(c(NA, special)), list(c(NA, vector[1]))) )
        }
        names(info) <- c("package","name")
        list(info)
    }
    for (arg in args) {
        if (is.character(arg)) {
            x <- strsplit(arg, ":::")[[1]]
            y <- strsplit(arg, "::")[[1]]
            if (length(x) == 1 & nchar(x[1]) > 0) {
                res <- c(res, getInfo(y, arg, "::"))
            } else {
                res <- c(res, getInfo(x, arg, ":::"))
            }
        } else {
            stop("Function names should be provided as character string!")
        }
    }
    res
}

#' Returns names of functions defined in given file(s)
#'
#' @description Analyses given file, or files if directory
#' is supplied for all functions defined in global scope and returns their names as character vector.
#'
#' @param src.root A source file to be analyzed, or a directory containing source files (*.R or *.r) to be analyzed.
#' @param recursive TRUE if subdirectories should be scanned too.
#' @return Character vector of function names defined in the file.
list_functions <- function(src.root, recursive = TRUE)
{
    functions = character()
    if (file.info(src.root)$isdir)
        src.root <- list.files(src.root, pattern = "[rR]$", recursive = recursive, full.names = T)
    for (src.file in src.root) {
        exp <- parse(src.file)
        for (e in exp) {
            if (typeof(e) == "language" && e[[1]] == as.name("<-") && is.name(e[[2]])) {
                name <- e[[2]]
                what <- e[[3]]
                if (typeof(what) == "language" && what[[1]] == as.name("function")) {
                    functions = c(functions, as.character(name))
                }
            }
        }
    }
    functions
}

split_path <- function(path)
{
    setdiff(strsplit(path,"/|\\\\")[[1]], "")
}

extract_example <- function(ex)
{
    sapply(ex, function(x) x[[1]])
}

example_code <- function(fromFile)
{
    code <- tools::parse_Rd(fromFile)
    code <- code[sapply(code, function(x) attr(x, "Rd_tag") == "\\examples")]
    result = ""
    for (cc in code)
        result = c(result, extract_example(cc))
    result
}

