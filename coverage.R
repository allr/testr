#' Report coverage statistics
#' Parameters:
#'  * dir: the directory where .gcno files for the VM source code are stored or a single C file.
#'         The VM must have been compiled with gcov support and executed at least once.
#'  * exclude.header: TODO
#'  * file/func.detail: TODO
#'  * file/func.keyword: TODO
#'  * ignore.case: as name sugguests
#' Return: list(file=file.df, func=func.df)

coverage <- function(dir, exclude.header=TRUE, file.detail=FALSE, func.detail=FALSE, file.keyword="", func.keyword="", ignore.case=TRUE, ...) {
  DEBUG <- FALSE;
  if (missing(dir)) stop("A directory containing VM source files must be specified!");
  if (DEBUG) cat("===",dir,"===","\n");
  if (DEBUG) cat("===",dir,"===","\n");
  if (length(grep("[.]c$", dir, ignore.case=TRUE))) { cfiles <- dir }
  else { cfiles <- list.files(path=dir, recursive=TRUE, pattern=".c$") }
  if (DEBUG) print(cfiles);
  res <- c()
  gcovOnSingleFile <- function(f) {
    if (DEBUG) cat("-----------------",f,"-----------------","\n");
    path <- paste(dir, dirname(f), sep="/");
    file <- paste(dir, f, sep="/");
    cmd <- paste("gcov", "-p", "-n", "-f",  "-o", path, file, sep=" ");
    r <- system(cmd, intern=TRUE);
    grepFileLine <- function(line) { grep("^File", line) }
    grepFuncLine <- function(line) { grep("^Function", line) }
    grepDataLine <- function(line) { grep("^Lines executed", line) }
    fileLines <- Filter(grepFileLine, r);
    funcLines <- Filter(grepFuncLine, r);
    dataLines <- Filter(grepDataLine, r);
    if (DEBUG) print(fileLines);
    if (DEBUG) print(funcLines);
    if (DEBUG) print(dataLines);
    extractFile <- function(line) { strsplit(line,"'")[[1]][2] }
    extractFunc <- function(line) { strsplit(line,"'")[[1]][2] }
    extractData <- function(line) { unlist(strsplit(strsplit(line,"Lines executed:")[[1]][2],"% of ")) }
    if (length(fileLines) != 0) {
      files.len <- length(fileLines);
      funcs.len <- length(funcLines);
      data.len  <- length(dataLines);
      if (files.len + funcs.len != data.len) stop("Unexpected gcov output format!");
      files <- unlist(lapply(fileLines, extractFile));
      funcs <- unlist(lapply(funcLines, extractFunc));
      data  <- unlist(lapply(dataLines, extractData));
      if (DEBUG) print(files);
      if (DEBUG) print(funcs);
      if (DEBUG) print(data);
      func.data <- data[1:(funcs.len*2)];
      file.data <- data[(funcs.len*2+1):(data.len*2)];
      if (DEBUG) print(func.data);
      if (DEBUG) print(file.data);
      func.matrix <- cbind(funcs, matrix(unlist(func.data), ncol=2, byrow=TRUE));
      file.matrix <- cbind(files, matrix(unlist(file.data), ncol=2, byrow=TRUE));
      if (DEBUG) print(func.matrix);
      if (DEBUG) print(file.matrix);
      func.df <- data.frame(func.matrix, stringsAsFactors=FALSE);
      file.df <- data.frame(file.matrix, stringsAsFactors=FALSE);
      colnames(func.df) <- c("Func", "CovLn%", "LOC");
      colnames(file.df) <- c("File", "CovLn%", "LOC");
      calCovLnAndAddObj <- function(df) {
        covLn <- round(as.numeric(df$'CovLn%') / 100.0 * as.numeric(df$'LOC'), digits=0);
        df <- cbind(df, 'CovLn'=covLn);
        cbind(df, Obj=f);        
      }
      func.df <- calCovLnAndAddObj(func.df);
      file.df <- calCovLnAndAddObj(file.df);
      func.df <- func.df[,c(5,1,4,3,2)]; # Obj Func CovLn LOC CovLn%
      file.df <- file.df[,c(5,1,4,3,2)]; # Obj File CovLn LOC CovLn%
      if (DEBUG) print(func.df);
      if (DEBUG) print(file.df);
      list(file=file.df, func=func.df);
    } else {
      NULL
    }
  }
  result <- Map(gcovOnSingleFile, cfiles);
  if (DEBUG) print(result);
  file.df <- data.frame('File'=vector(),'CovLn%'=vector(),'LOC'=vector(),'CovLn'=vector(),'Obj'=vector());
  func.df <- data.frame('Func'=vector(),'CovLn%'=vector(),'LOC'=vector(),'CovLn'=vector(),'Obj'=vector());
  for (item in result) {
    file.df <- rbind(file.df, item$file);
    func.df <- rbind(func.df, item$func);
  }
  if (DEBUG) print(file.df);
  if (DEBUG) print(func.df);
  file.df <- file.df[grep(file.keyword, file.df$'Obj', ignore.case=ignore.case),];
  func.df <- func.df[grep(file.keyword, func.df$'Obj', ignore.case=ignore.case),];
  func.df <- func.df[grep(func.keyword, func.df$'Func', ignore.case=ignore.case),];

  if (exclude.header) {
    file.df <- file.df[grep("[.]c$", file.df$'File', ignore.case=ignore.case),];  
  }
  if (DEBUG) print(file.df);
  if (DEBUG) print(func.df);

  # line file coverage
  totalLine.file        <- sum(as.numeric(file.df$'LOC'));
  totalCovLine.file     <- sum(as.numeric(file.df$'CovLn'));
  totalCovLinePcnt.file <- round(totalCovLine.file / totalLine.file * 100, digits=2);
  # line func coverage
  totalLine.func        <- sum(as.numeric(func.df$'LOC'));
  totalCovLine.func     <- sum(as.numeric(func.df$'CovLn'));
  totalCovLinePcnt.func <- round(totalCovLine.func / totalLine.func * 100, digits=2);
  # func coverage
  totalFunc        <- nrow(func.df);
  totalCovFunc     <- nrow(func.df[as.numeric(func.df$'CovLn')>0,]);
  totalCovFuncPcnt <- round(totalCovFunc / totalFunc * 100, digits=2);
  # file coverage
  totalFile        <- nrow(file.df);
  totalCovFile     <- nrow(file.df[as.numeric(file.df$'CovLn')>0,]);
  totalCovFilePcnt <- round(totalCovFile / totalFile * 100, digits=2);

  cat("========-------- Coverage Report --------========\n\n");
  cat(">>> Configration:\n\n");
  cat("- src diretory:     ", dir, "\n", sep="");
  cat("- file keyword:     ", file.keyword, "\n", sep="");
  cat("- func keyword:     ", func.keyword, "\n", sep="");
  cat("- igore case:       ", ignore.case, "\n", sep="");
  cat("- exclude header:   ", exclude.header, "\n", sep="");
  cat("\n");
  cat(">>> Coverage:\n\n");
  cat("* Line (file): ", totalCovLine.file, " out of ", totalLine.file, " (", totalCovLinePcnt.file, "%)\n", sep="");
  cat("* Line (func): ", totalCovLine.func, " out of ", totalLine.func, " (", totalCovLinePcnt.file, "%)\n", sep="");
  cat("* File:        ", totalCovFile,      " out of ", totalFile,      " (", totalCovFilePcnt,      "%)\n", sep="");
  cat("* Func:        ", totalCovFunc,      " out of ", totalFunc,      " (", totalCovFuncPcnt,      "%)\n", sep="");

  if (file.detail) {
    cat("\n----------------   File Detail   ----------------\n\n");
    print(file.df, row.names=FALSE);
  }
  if (func.detail) {
    cat("\n----------------   Func Detail   ----------------\n\n");
    print(func.df, row.names=FALSE);
  }
  cat("\n=================================================\n\n");
  return (list(file=file.df, func=func.df));
}

#coverage("/home/lzhao/r/gcov/src")
#coverage("/home/lzhao/r/R-3.0.1/src/main")
