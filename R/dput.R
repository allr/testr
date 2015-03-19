#' @export
dputModified <- function (x, file = "", control = c("keepNA", "keepInteger", 
                                                    "showAttributes")) 
{
  res <- lapply(x, function(l) capture.output(dputModifiedHelper(l, file, control)))
  print(res)
}

dputModifiedHelper <- function (x, file = "", control = c("keepNA", "keepInteger", 
                                                          "showAttributes")) 
{
  if (is.character(file)) 
    if (nzchar(file)) {
      file <- file(file, "wt")
      on.exit(close(file))
    }else file <- stdout()
  opts <- .deparseOpts(control)
  if (isS4(x)) {
    clx <- class(x)
    cat("new(\"", clx, "\"\n", file = file, sep = "")
    for (n in .slotNames(clx)) {
      cat("    ,", n, "= ", file = file)
      slot.obj <- slot(x, n)
      if (is.language(slot.obj))
        dput(enquote(slot(x, n)), file = file, control = control)
      else 
        dput(slot(x, n), file = file, control = control)
    }
    cat(")\n", file = file)
    invisible()
  } 
  else {
    if (is.language(x))
      x <- enquote(x)
    if (is.list(x) || is.vector(x)) x <- lapply(x, function(x) if(is.language(x)) enquote(x) else x)
    .Internal(dput(x, file, opts))
  } 
}