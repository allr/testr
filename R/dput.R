#' @export
#' 
dputModified <- function(x) {
  res <- capture.output(dputModifiedHelper(x))
  res
}

dputModifiedHelper <- function (x,
                   file = "",
                   control = c("keepNA", "keepInteger", "showAttributes")){
  if (is.character(file))
    if (nzchar(file)) {
      file <- file(file, "wt")
      on.exit(close(file))
    }
  else file <- stdout()
  opts <- .deparseOpts(control)
  if (isS4(x)) {
    cat("new(\"", class(x), "\"\n", file = file, sep = "")
    for (n in slotNames(x)) {
      cat("    ,", n, "= ", file = file)
      if (is.language(slot.obj))
        dputModified(enquote(slot(x, n)), file = file, control = control)
      else 
        dputModified(slot(x, n), file = file, control = control)
    }
    cat(")\n", file = file)
    invisible()
  } else if(length(grep('@',capture.output(str(x)))) > 0){
    if(is.list(x) || is.vector(x)){
      is.first <- TRUE
      cat("list(\n", file = file, sep = "")
      for (i in 1:length(x)) {
        if(!is.null(names(x))){
          n <- names(x)[i]
          if(n != ''){
            if (is.first) {
              cat(n, "= ", file = file)
              is.first <- FALSE
            } else 
              cat(" , ", n, "= ", file = file)
          }
        }
        dputModified(x[[i]], file = file, control = control)
      }
      cat(")\n", file = file)
      invisible()
    } else {
      stop('S4 objects are only handled if they are contained within an S4 object or a list/vector object')
    }
  }
  else {
    if (is.language(x))
      x <- enquote(x)
    if (is.list(x) || is.vector(x)) x <- lapply(x, function(x) if(is.language(x)) enquote(x) else x)
    .Internal(dput(x, file, opts))
  }
}