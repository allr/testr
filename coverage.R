
#' Report coverage statistics 
#' dir: the directory where .gcno files for the VM source code are stored.
#' The VM must have been compiled with gcov support and executed at least once.

coverage <- function(dir=NULL) {
  if (missing(dir)) return(0);
  cfiles <- list.files(path=dir, recursive=TRUE, pattern=".c$")
  res <- c()
  gcovOnSingleFile <- function(f) {
    #print("-----------------")
    path <- paste(dir, dirname(f), sep="/")
    file <- paste(dir, f, sep="/")
    cmd <- paste("gcov", "-p", "-n",  "-o", path, file, sep=" ")
    r <- system(cmd, intern=TRUE)
    isFileLine <- function(line) { grep("File", line) == 1 }
    isPercentageLine <- function(line) { grep("Lines executed", line) == 1 }
    fileLines <- Filter(isFileLine, r)
    percentageLines <- Filter(isPercentageLine, r)
    #print(fileLines)
    #print(percentageLines)
    getFileName <- function(line) { strsplit(line,"'")[[1]][2] }
    getPercentage <- function(line) { 
        sub <- strsplit(line,"Lines executed:")[[1]][2]
        unlist(strsplit(sub,"% of "))
    }
    files <- lapply(fileLines, getFileName)
    percentage <- lapply(percentageLines, getPercentage)
    #print(files)
    #print(percentage)
    if (length(files) != 0) {
        data <- cbind(unlist(files), matrix(unlist(percentage), ncol=2, byrow=TRUE));
        #print(data);
        m <- data.frame(data, stringsAsFactors=FALSE);
        colnames(m)[1] <- "File";
        colnames(m)[2] <- "CovLn%";
        colnames(m)[3] <- "LOC";
        m;
    } else {
        NULL
    }
  }
  res <- Map(gcovOnSingleFile, cfiles)
  covTable <- data.frame(File=character(),'CovLn%'=character(),LOC=character());
  #print(sapply(covTable, mode));
  #print(sapply(covTable, class));
  #print(sapply(covTable, typeof));
  for (df in res) {
    covTable <- rbind(covTable, df);
  }
  covLCol <- as.numeric(covTable$'CovLn%') / 100.0 * as.numeric(covTable$LOC);
  covTable <- cbind(covTable, CovLn=covLCol);
  totalLOC <- sum(as.numeric(covTable$LOC));
  totalCovLn <- sum(as.numeric(covTable$CovLn));
  totalCovLnPcnt <- totalCovLn / totalLOC * 100;
  cat("========-------- Coverage Report --------========\n");
  cat("\n");
  cat("* Src diretory: ", dir, "\n", sep="");
  cat("* Line coverage: ", totalCovLn, " out of ", totalLOC, " (", totalCovLnPcnt, "%)\n", sep="");
  cat("\n");
  cat("=================================================\n");
  cat("\n");
  input <- readline("(press d for details or any other keys to exit)\n");
  cat("\n");
  if (input == "d") print(covTable);
 }

#coverage("/home/lzhao/r/gcov/src")
#coverage("/home/lzhao/r/R-3.0.1/src/main")
