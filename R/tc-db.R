OrginizeTestCases <- function(root, res.dir){
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=".[rR]$", recursive = TRUE, all.files = TRUE) 
    files <- Map(function (x) paste(root,"/",x, sep=""), files) 
  } else {
    files <- root
  }
  if (!file.exists(res.dir))
    dir.create(res.dir)
  cache <- new.env();
  for (filename in files) {
    cat(filename, "\n")
    function.name <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
    function.name <- gsub("\\.", "", function.name)
    function.name <- gsub("<-", "assign", function.name)
    if (function.name %in% operators){
      function.name <- "operators"
    }      
    f.number <- cache[[function.name]]
    if (is.null(f.number)){
      f.number <- 1
      dir.create(paste(res.dir, function.name, sep="/"))      
    }
    res.file.name <- paste(res.dir, function.name, paste("tc_", function.name, "_", f.number, ".R", sep=""), sep="/")
    lines <- readLines(filename);
    res.conn <- file(res.file.name, "w")
    write(lines, res.conn, append=TRUE)
    close(res.conn)
    cache[[function.name]] <- f.number + 1
  }
}

RemoveDuplicates <- function(root){
  TestGetArgs <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
    as.list(substitute(code))[2]   
  }
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=".[rR]$", recursive = TRUE, all.files = TRUE, full.names = TRUE) 
  } 
  args.cache <- new.env() 
  temp.env <- new.env()
  for (filename in files){
    function.name <- ExtractFunctionName(filename)
    args.list <- args.cache[[function.name]]
    if (is.null(args.list)) {
      args.list <- list()
    }
    temp.env$test <- TestGetArgs
    with(temp.env, argv <- source(filename, local = TRUE)$value)
    argv <- temp.env$argv
    repeated <- FALSE
    for (saved.argv in args.list){
      if (identical(all.equal(argv, saved.argv), TRUE)) {
        repeated <- TRUE
        break
      }
    }
    if (!repeated) {
      if (is.null(argv))
        args.list <- c(args.list, list(NULL))
      else
        args.list <- c(args.list, argv)
      args.cache[[function.name]] <- args.list
    } else {
      file.remove(filename)
    }
  }
}