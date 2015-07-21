#' @export
#' @title _coverage Report on Specified R Virtual Machine Source Code
#'
#' @description This function works with the GNU coverage tool, gcov, to report code coverage of the
#'  tested R virtual machine. and The VM must have been compiled with gcov support and executed at least
#'  once before this function is executed for meanful statistics. Note that, gcov is designed such
#'  that the coverage results are accumulated, so for a fresh new result of a test suit, call
#'  \code{\link{reset}} before executing the test suite. The coverage granularition spans line, function,
#'  and file. 
#'
#' @param root a directory or a single C file that contains or is instrumented VM source.
#' @param verbose wheather to display resulting info
#' @param exclude_header whether to include header files in statistics.
#' @param file_detail whether to display statistic details for files. Only works if verbose is true.
#' @param func_detail whether to display statistic details for functions. Only works if verbose is true.
#' @param file_keyword filtering the result by only including files whose names contain the keyword.
#' @param func_keyword filtering the result by only including functions whose names contain the keyword.
#' @param ignore_case whether the keywords should be case-sensitive.
#' @return list(file=file_df, func=func_df)
#' @details In this section we give some design thoughts then in example section some sample
#' reports and the interpretation.
#' 
#' As a testcase contributor, the right way to use the coverage rate is to look for increments
#' after new testcases are added rather than aiming for 100% coverage. The reason is that, due
#' to the file inclusion feature of C language, exhaustive tests will be hardly shown as 100%
#' coverage (could be lower or higher). Specifically, if one piece of code in a header file gets
#' included in two C files, gcov reports it twice and C in C inclusion will make the situation
#' even more complicated. So we give users the choices to either include both (over estimation)
#' or include none (under estimation) in the overall result by allowing them to choose whether
#' to include the header files in the statistics. Identifying duplication is almost impossible.
#'
#' On the other hand, gcov produces precise result object-wise. The term "object" is used as is
#' in "object file" during linking, referring to a group of files glued together by inclusion,
#' usually a C file and several header files but in rare case several C files and several header
#' files. The coverage within the range of object is precise as C compilers guarantee no duplication
#' by nature. In that, the object information is included in the file/function detail tables for
#' users' reference.
#'
measure_gcov <- function(root, verbose = TRUE, exclude_header=TRUE, file_detail=FALSE,
                         func_detail=FALSE, file_keyword="", func_keyword="", ignore_case=TRUE) {
    if (missing(root)) {
        stop("A directory containing VM source files must be specified!")
    }
    if (length(root) == 0 || !file.exists(root)) {
        return(list(file=0, func=0, file.pcn=0, func.pcn=0))
    }
    if (.Platform$OS.type == "windows") {
        stop("Not supported on Windows!")
    }
    if (length(grep("[.]c$", root, ignore.case=TRUE))) {
        cfiles <- root
    } else {
        if (!length(list.files(path=root, recursive=TRUE, pattern=".gcda$"))) {
            stop("No coverage information in the specified folder! .gcda files do not exist")
        }
        cfiles <- list.files(path=root, recursive=TRUE, pattern=".c$")
    }
    if (!length(cfiles)) {
        stop("Root param has no files with .c extension")
    }

    gcov_file <- function(f) {
        path <- file.path(root, dirname(f), fsep=.Platform$file.sep)
        file <- file.path(root, f,          fsep=.Platform$file.sep)
        cmd <- paste("gcov", "-p", "-n", "-f",  "-o", path, file, sep=" ")
        r <- system(cmd, intern=TRUE, ignore.stderr=TRUE)
        # for gcov4.7 output
        i <- which(r == "No executable lines")
        j <- i - 1
        i <- append(i,j)
        if (length(i) != 0)
            r <- r[-i]
        file_lines <- r[grep("^File", r)]
        func_lines <- r[grep("^Function", r)]
        data_lines <- r[grep("^Lines executed", r)]
        if (length(file_lines) != 0) {
            files_len <- length(file_lines)
            funcs_len <- length(func_lines)
            data_len  <- length(data_lines)
            if (files_len + funcs_len != data_len) stop("Unexpected gcov output format!")
            files <- matrix(unlist(strsplit(file_lines,"'")), ncol=2, byrow=TRUE)[,2]
            funcs <- matrix(unlist(strsplit(func_lines,"'")), ncol=2, byrow=TRUE)[,2]
            data  <- strsplit(unlist(strsplit(data_lines, "Lines executed:")), "% of ")[seq(2,2 * data_len,2)]
            func_data <- data[1:funcs_len]
            file_data <- data[ (funcs_len + 1):data_len]
            func_df <- data.frame(cbind(funcs, matrix(unlist(func_data), ncol=2, byrow=TRUE)), stringsAsFactors=FALSE)
            file_df <- data.frame(cbind(files, matrix(unlist(file_data), ncol=2, byrow=TRUE)), stringsAsFactors=FALSE)
            colnames(func_df) <- c("Func", "CovLnPcnt", "LOC")
            colnames(file_df) <- c("File", "CovLnPcnt", "LOC")
            cal_cov_ln <- function(df) {
                cov_ln <- round(as.numeric(df$"CovLnPcnt") / 100.0 * as.numeric(df$LOC), digits=0)
                df <- cbind(df, CovLn=cov_ln)
                cbind(df, Obj=f)
            }
            func_df <- cal_cov_ln(func_df)[,c(5,1,4,3,2)] # Obj _func _covLn LOC _covLn%
            file_df <- cal_cov_ln(file_df)[,c(5,1,4,3,2)] # Obj _file _covLn LOC _covLn%
            list(file=file_df, func=func_df)
        } else {
            NULL
        }
    }

    result <- Map(gcov_file, cfiles)
    file_df <- data.frame(File=vector(),
                          CovLnPcnt=vector(),
                          LOC=vector(),
                          CovLn=vector(),
                          Obj=vector())
    func_df <- data.frame(Func=vector(),
                          CovLnPcnt=vector(),
                          LOC=vector(),
                          CovLn=vector(),
                          Obj=vector())
    for (item in result) {
        file_df <- rbind(file_df, item$file)
        func_df <- rbind(func_df, item$func)
    }
    file_df <- file_df[grep(file_keyword, file_df$"Obj", ignore.case=ignore_case),]
    func_df <- func_df[grep(file_keyword, func_df$Obj, ignore.case=ignore_case),]
    func_df <- func_df[grep(func_keyword, func_df$Func, ignore.case=ignore_case),]
    if (exclude_header) {
        file_df <- file_df[grep("[.]c$", file_df$File, ignore.case=ignore_case),]
    }

    # line file coverage
    total_line_file        <- sum(as.numeric(file_df$"LOC"))
    total_cov_line_file     <- sum(as.numeric(file_df$"CovLn"))
    total_cov_line_pcnt_file <- round(total_cov_line_file / total_line_file * 100, digits=10)
    # line func coverage
    total_line_func        <- sum(as.numeric(func_df$"LOC"))
    total_cov_line_func     <- sum(as.numeric(func_df$"CovLn"))
    total_cov_line_pcnt_func <- round(total_cov_line_func / total_line_func * 100, digits=10)
    # func coverage
    total_func        <- nrow(func_df)
    total_cov_func     <- nrow(func_df[as.numeric(func_df$"CovLn") > 0,])
    total_cov_func_pcnt <- round(total_cov_func / total_func * 100, digits=2)
    # file coverage
    total_file        <- nrow(file_df)
    total_cov_file     <- nrow(file_df[as.numeric(file_df$"CovLn") > 0,])
    total_cov_file_pcnt <- round(total_cov_file / total_file * 100, digits=2)

    if (verbose){
        cat("========-------- Coverage Report --------========\n")
        cat("\n")
        cat(">>> Configration:\n")
        cat("\n")
        cat("- src root:         ", root,           "\n", sep="")
        cat("- file keyword:     ", file_keyword,   "\n", sep="")
        cat("- func keyword:     ", func_keyword,   "\n", sep="")
        cat("- ignore case:       ", ignore_case,    "\n", sep="")
        cat("- exclude header:   ", exclude_header, "\n", sep="")
        cat("\n")
        cat(">>> Coverage:\n")
        cat("\n")
        cat("* Line (file): ",
            total_cov_line_file, " out of ", total_line_file,
            " (", total_cov_line_pcnt_file, "%)\n", sep="")
        cat("* Line (func): ",
            total_cov_line_func, " out of ", total_line_func,
            " (", total_cov_line_pcnt_func, "%)\n", sep="")
        cat("* File:        ",
            total_cov_file,      " out of ", total_file,
            " (", total_cov_file_pcnt,      "%)\n", sep="")
        cat("* Func:        ",
            total_cov_func,      " out of ", total_func,
            " (", total_cov_func_pcnt,      "%)\n", sep="")
        if (file_detail) {
            cat("\n")
            cat("----------------   File Details   ----------------\n")
            cat("\n")
            print(file_df, row.names=FALSE)
        }
        if (func_detail) {
            cat("\n")
            cat("----------------   Func Detail   ----------------\n")
            cat("\n")
            print(func_df, row.names=FALSE)
        }
        cat("\n")
        cat("=================================================\n")
    }
    return (list(file=file_df,
                 func=func_df,
                 file_pcnt = total_cov_line_pcnt_file,
                 func_pcnt = total_cov_line_pcnt_func))
}

#' @export
#' @title Reset coverage information
#'
#' @description This function deletes gcov information files (*.gcda) 
#' thus clearing any previously collected coverage information is erased.
#' @param root a directory or a single C file that contains or is instrumented VM source.
#'
clear_gcov <- function(root) {
    cat("clear_gcov called\n")
    invisible(file.remove(list.files(root, pattern="*\\.gcda",
                                    recursive = TRUE, full.names = TRUE)))
}
