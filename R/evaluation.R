CranTester <- function() {
  
}

CapturePackage <- function(name, dir, funcs) {
  BeginBuiltinCapture(functions = funcs)  
  loc <- download.packages(name, dir, type = "source")[,2]
  untar(loc, exdir = dir)
  file.remove(loc)
  loc <- file.path(dir, name)
  devtools::install_deps(loc, T)
  devtools::run_examples(loc)
  devtools::test(loc)
  # something to run vignette
#   unlink(loc, recursive = T)
}