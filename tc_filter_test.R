setwd("~/Documents/RProject/")
tc.db.path <- "~/Documents/RProject/testr/tc_db/" #should be an absolute path
tc.dir.path <- "~/Documents/RProject/wd/tc/" #should be an absolute path
r.home <- "~/Documents/RProject/R-3.0.1/"
source.folder <- "src/main"
source("~/Documents/RProject/testr/tc-filter.r")
filterTCs(tc.root=tc.dir.path,r.home, source.folder, tc.db.path)