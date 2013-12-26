setwd("~/rWD/")
tc.db.path <- "~/rWD/resulting_tc_db/" #should be an absolute path
tc.dir.path <- "~/rWD/tc_db/" #should be an absolute path
r.home <- "~/Documents/RProject/R-3.0.1/"
source.folder <- "src/main"
source("~/rWD/testr/tc-filter.r")
filterTCs(tc.root=tc.dir.path,r.home, source.folder, tc.db.path)