#' @title ReplaceS4 methods in the environment
#' 
#' This functions takes decorates S4 methods in the environment
#'
#' @param env environment to search for S4 methods
#' @seealso ReplaceBody
ReplaceS4 <- function(env) {
  generics <- getGenerics(env)
  
  replacer <- function(x, envir) {
    target <- get(x, envir = envir)
    if (!is.null(target)) {
      do.call(trace, list(target, quote(print("tracing"))))
#       new.value <- testr:::Decorate(target)
    }
  }
  
  unlist(recursive = FALSE,
         Map(generics@.Data, generics@package, USE.NAMES = FALSE,
             f = function(name, package) {
               what <- methodsPackageMetaName("T", paste(name, package, sep = ":"))
               table <- get(what, envir = env)
               lapply(ls(table, all.names = TRUE), replacer, table)
             })
  )
}