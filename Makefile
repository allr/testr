NEWS     = NEWS
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

build:
    cd ..;\
    R CMD build $(PKGSRC)

install: build
    cd ..;\
    R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
    cd ..;\
    R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

test:
    R -e 'library(testr);testr_options("rtests", TRUE); library(testthat);test_package("testr")'