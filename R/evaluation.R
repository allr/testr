#' @title Capture run information from CRAN packages
#' 
#' This function is responsible for getting all possible capture information from CRAN
#' It tries to insstall package and run tests, examples and vignettes
#' @param indexes indexes of specific packages
#' @param funcs functions to Decorate
#' @export
#'
CranTester <- function(indexes=1:10000, funcs) {
  ap <- available.packages()[indexes,1]
  sapply(ap, CapturePackage, funcs = funcs)
}

#' @title Capture run information from Bioconductor packages
#' 
#' This function is responsible for getting all possible capture information from Bioconductor
#' It tries to insstall package and run tests, examples and vignettes
#' @param indexes indexes of specific packages
#' @param funcs functions to Decorate
#' @export
#'
BioconductorTester <- function(indexes=1:1000, funcs) {
  contriburl <- paste(biocinstallRepos()["BioCsoft"], "src/contrib", 
                      sep = "/")
  ap <- available.packages(contriburl)[indexes,1]
  sapply(ap, CapturePackage, from.bioc = T, contriburl = contriburl, funcs = funcs)
}

#' @title Capture run information from package
#' 
#' This function is responsible for getting all possible capture information from specific package. 
#' It tries to insstall package and run tests, examples and vignettes
#' @param name function name as a character string
#' @param dir function name as a character string
#' @param from.bioc if package is from Bioconductior
#' @param contribur contributor url as in download.packages
#' @param funcs functions to Decorate
#' @export
#'
CapturePackage <- function(name, dir=tempdir(), from.bioc = FALSE, contriburl, funcs) {
  BeginBuiltinCapture(functions = funcs)  
  if (!missing(contriburl))
    loc <- suppressMessages(download.packages(name, dir, contriburl = contriburl, type = "source")[,2])
  else 
    loc <- suppressMessages(download.packages(name, dir, type = "source")[,2])
  untar(loc, exdir = dir)
  if (!missing(dir)) file.remove(loc)
  loc <- file.path(dir, name)
  
  cat("===Intalling package - ", name, "\n")
  if (from.bioc) 
    biocLite(name)
  else
    devtools::install(pkg = loc, quiet = T, dependencies = T)
  
  cat("===Running examples\n")
  capture.output(suppressMessages(devtools::run_examples(loc)))
  
  cat("===Running tests\n")
  capture.output(suppressMessages(devtools::test(loc)))
  
  cat("===Running vignettes\n")
  info <- tools:::getVignetteInfo(package = name)
  vdir <- info[,2]
  vfiles <- info[,6]
  p <- file.path(vdir, "doc", vfiles)
  capture.output(suppressMessages(sapply(p, source)))
  
  invisible()
}

