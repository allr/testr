# Copyright (c) 2013, Purdue University. All rights reserved.
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
#
# This code is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 only, as
# published by the Free Software Foundation.
#
# This code is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# version 2 for more details (a copy is included in the LICENSE file that
# accompanied this code).
#
# You should have received a copy of the GNU General Public License version
# 2 along with this work; if not, write to the Free Software Foundation,
# Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
# or visit www.oracle.com if you need additional information or have any
# questions.

join <- function(separator, v) {
    if (length(v) == 0) {
        ""
    } else {
        result = as.character(v[[1]])
        for (i in (v)[-1])
            result = paste(result, separator, i, sep = "") 
        result
    }
}


command <- function(executable, arguments = NULL, input = NULL, workingDirectory = NULL) {
    wd <- getwd()
    if (!missing(workingDirectory))
        setwd(workingDirectory)
    if (missing(arguments))
        cmd <- shQuote(executable)
    else
        cmd <- join(" ", c(shQuote(executable), arguments))
    suppressWarnings(
        time <- system.time(tmp <- system(cmd, intern = TRUE, input = input, show.output.on.console = FALSE))
    )
    if (!is.null(wd))
        setwd(wd)
    result <- list(
        status = attr(tmp, "status"),
        user.self = time[[1]], # maybe we do not need these
        sys.self = time[[2]], # maybe we do not need these
        elapsed = time[[3]],
        user.child = time[[4]], # maybe we do not need these
        sys.child = time[[5]], # maybe we do not need these
        output = tmp,
        command = cmd
    )
    if (is.null(result$status))
        result$status <- 0
    attributes(result$output) <- NULL
    class(result) <- "commandResult"
    result
}

print.commandResult <- function(tr) {
    cat("Command:     ", tr$command, "\n")
    if (tr$status == 0)
        cat("Status:      ", tr$status, "OK\n")
    else
        cat("Status:      ", tr$status, "FAILURE\n")
    cat("Elapsed time:", tr$elapsed, "[sec]\n")
    cat("Output:       (", length(tr$output), " lines)\n", sep="")
    for (i in tr$output)
        cat("             ",i,"\n")
}

is.commandResult <- function(o) {
    inherits(o, "commandResult")
}



RPATH = "c:\\Program Files\\R\\R-3.0.1\\bin\\R.exe"