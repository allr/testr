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

  devtools::load_all(pkg = loc, quiet = T)
  if (missing(funcs)) 
    funcs <- ls(getNamespace(name))
  cat("===Inserting Trace points")
  BeginBuiltinCapture(functions = funcs, package = name)  

  cat("===Running examples\n")
  capture.output(suppressMessages(PackageRunExamples(name)))
  
  cat("===Running tests\n")
  capture.output(suppressMessages(PackageRunTests(loc)))
  
  cat("===Running vignettes\n")
  capture.output(suppressMessages(PackageRunVignettes(name)))
  
  ClearDecoration()
  invisible()
}

#' @title Run all examples in the package
#' 
#' This function is responsible for running all examples in specified package
#' @param package package name
PackageRunExamples <- function(package) {
  invisible(sapply(ls(getNamespace(package)), function(x) do.call(example, list(x))))
}

#' @title Run testthat tests in the package
#' 
#' @description This function is responsible for running testthat tests for specified package
#' @param loc location of the package source
PackageRunTests <- function(loc) {
  test_path <- FindTestDir(loc)
  test_files <- dir(test_path, "^test.*\\.[rR]$")
  library(testthat, quietly = TRUE)
  testthat::test_dir(test_path)
}

#' @title Run all vignettes in the package
#' 
#' This function is responsible for running code from vignettes for specified package
#' @param name package name
PackageRunVignettes <- function(name) {
  info <- tools:::getVignetteInfo(package = name)
  vdir <- info[,2]
  vfiles <- info[,6]
  p <- file.path(vdir, "doc", vfiles)
  invisible(sapply(p, source))
}
#' @title Run specific code in the file and generated test cases
#'
#' @description This function runs the code in give source files and tries to generate test cases
#' from collected trace information.
#'
#' @param src.root source root or source file
#' @param tc.result.root destination of generated test cases
#' @param functions functions to be traced
#' @export
RunCodeGenerateTests <- function(src.root, tc.result, functions) {
  if (!file.exists(src.root))
    stop("Supplied source does not exist")
  if (file.info(src.root)$isdir)
    src.root <- list.files(src.root, pattern = "\\[rR]", recursive = T, full.names = T)
  BeginBuiltinCapture(functions = functions)
  for (src.file in src.root)
    source(src.file, local = T)
  ClearDecoration()
  TestGen(kCaptureFolder, tc.result)
  invisible()
}
