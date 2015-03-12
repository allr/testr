#' @title Redo the test cases of LM
#' 
#' This function is respinsible 
#' @param r.test.root folder with R testcases
#' @param fastr.test.folder folder with FastR test cases in Java
#'
LmTranslate <- function(r.test.folder, result.folder = "lm_f/"){
  # unility function for extraction of test case information. Replaces o
  LmTest <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
    str <- as.list(substitute(code))[-1] # extract statements from code and replace double quotes with single ones   
    str <- lapply(str, function(x) paste(deparse(x), collapse = "\n"))
    error <- FALSE
    res <- tryCatch(eval(parse(text=str)), error = function(x) error <<- TRUE)
    if (!error) {
      if (!is.null(res$call)) res$call <- enquote(res$call)
      rstr <- paste("\nexpected <-", paste(deparse(res), collapse = "\n"))
      rstr <- paste(rstr, sprintf("test(id=%i, code={\n%s}, o = expected)", id, paste(str, collapse = "\n")), sep="\n")
      filename <- sprintf("%s_%i.R", GetFunctionName(basename(filename)), k)
      k <<- k + 1
      cat(rstr, file = file.path(result.folder, filename), append=T)
    }
  }

  # test.folder sanity check
  if (!file.exists(result.folder)){
    dir.create(result.folder)
  }
  k <- 1
  r.test.files <- GetAllFiles(r.test.folder)
  # cache for storing information about functions (code and test case count)
  function.cache <- list()
  
  for (filename in r.test.files) {
    temp.env <- new.env();
    temp.env$test <- LmTest
    with(temp.env, res <- source(filename, local = TRUE)$value)
  }
}