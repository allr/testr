#' @title Capture run information from CRAN packages
#' 
#' This function is responsible for getting all possible capture information from CRAN
#' It tries to insstall package and run tests, examples and vignettes
#' @param indexes indexes of specific packages
#' @param funcs functions to Decorate
#' @export
#'
CranPackageGen <- function(name, dir, funcs=NULL, indexes = 1:10000) {
  if (!missing(name))
    ap <- name 
  else 
    ap <- available.packages()[indexes,1]
  sapply(ap, TestGenPackage, gen.dir = dir, funcs = funcs)
}

#' @title Capture run information from Bioconductor packages
#' 
#' This function is responsible for getting all possible capture information from Bioconductor
#' It tries to insstall package and run tests, examples and vignettes
#' @param indexes indexes of specific packages
#' @param funcs functions to Decorate
#' @export
#'
BioconductorPackageGen <- function(name, dir, funcs=NULL, indexes = 1:1000) {
  contriburl <- paste(biocinstallRepos()["BioCsoft"], "src/contrib", 
                      sep = "/")
  if (!missing(name))
    ap <- name 
  else 
    ap <- available.packages(contriburl)[indexes,1]
  sapply(ap, TestGenPackage, from.bioc = T, contriburl = contriburl, funcs = funcs, gen.dir = dir)
}

#' @title Capture run information from package and generate test cases
#' 
#' This function is responsible for getting all possible capture information from specific package. 
#' It tries to insstall package and run tests, examples and vignettes
#' @param name function name as a character string
#' @param dir function name as a character string
#' @param from.bioc if package is from Bioconductior
#' @param contribur contributor url as in download.packages
#' @param funcs functions to Decorate
#' @param gen if generate test cases
#' @export
#'
TestGenPackage <- function(name, gen.dir, funcs, from.bioc = FALSE, contriburl) {
  dir <- tempdir()
  if (!missing(contriburl))
    loc <- suppressMessages(download.packages(name, dir, contriburl = contriburl, type = "source")[,2])
  else 
    loc <- suppressMessages(download.packages(name, dir, type = "source")[,2])
  untar(loc, exdir = dir)
  if (!missing(dir)) file.remove(loc)
  loc <- file.path(dir, name)
  cat("===Intalling package - ", name, "\n")
  if (name %in% loadedNamespaces())
    tryCatch(detach(name=paste("package", name, sep=":"),unload = T, character.only = T),
             error=function(x) invisible())
  if (from.bioc) 
    biocLite(name)
  else
    install.packages(name, quiet = T, dependencies = T)
  if (!do.call(require, list(name))) {
    cat("===Package Loading Failed\n")
    return(invisible())
  }
  if (!file.exists(gen.dir) || !file.info(gen.dir)$isdir) dir.create(gen.dir)
  cat("===Inserting Trace points\n")
  if (missing(funcs) || is.null(funcs)) {
    funcs <- ls(getNamespace(name))
    SetupCapture(funcs, name)
  } else {
    SetupCapture(funcs)
  }
  hs <- help
  inv <- function(x) invisible()
  ReassignInEnv('help', inv, getNamespace('utils'))
  ReassignInEnv('help', inv, as.environment('package:utils'))
  
  cat("===Running examples\n")
  capture.output(PackageRunExamples(loc))
  cat("===Running tests\n")
  capture.output(PackageRunTests(loc))
  cat("===Running vignettes\n")
  capture.output(PackageRunVignettes(name))
  
  ReassignInEnv('help', hs, getNamespace('utils'))
  ReassignInEnv('help', hs, as.environment('package:utils'))
  cat("===Removing trace points\n")
  ClearDecoration()
  cat("===Generating tests\n")
  TestGen("capture", file.path(gen.dir, name))
  file.remove(list.files("capture", recursive = T, full.names = T))
  invisible()
}

#' @title Run all examples in the package
#' 
#' This function is responsible for running all examples in specified package
#' @param package package name
PackageRunExamples <- function(package) {
  pkg <- devtools:::as.package(package)
  files <- devtools:::rd_files(pkg)
  if (length(files) == 0) 
    return()
  tryCatch(lapply(files, devtools:::run_example), error=function(x) print(x))
}

#' @title Run testthat tests in the package
#' 
#' @description This function is responsible for running testthat tests for specified package
#' @param loc location of the package source
PackageRunTests <- function(loc) {
  test_path <- FindTestDir(loc)
  if (is.null(test_path))
    return(invisible())
  test_files <- dir(test_path, "^test.*\\.[rR]$")
  library(testthat, quietly = TRUE)
  tryCatch(testthat::test_dir(test_path), error=function(x) invisible())
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
  invisible(tryCatch(sapply(p, source),
                     error=function(x) invisible()))
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
TestGenSrc <- function(src.root, tc.result, functions) {
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

#' @title Run specific code in the file and generated test cases
#'
#' @description This function runs the code in give source files and tries to generate test cases
#' from collected trace information.
#'
#' @param src.root source root or source file
#' @param tc.result.root destination of generated test cases
#' @param functions functions to be traced
#' @export
TestGenCode <- function(code, tc.result, functions) {
  code <- substitute(code)
  BeginBuiltinCapture(functions = functions)
  eval(code)
  ClearDecoration()
  TestGen(kCaptureFolder, tc.result)
  invisible()
}