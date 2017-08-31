testR - test case generation for R
=====

[![Build Status](http://build.renjin.org/job/Test-Generation/job/testr/badge/icon)](http://build.renjin.org/job/Test-Generation/job/testr/)

TestR is a framework for unit tests generation from source code and for test execution, and filtering of test cases based on C code coverage using `gcov` and R code coverage using `rcov` (https://github.com/allr/rcov).

This is the testR-py reimplementation and extension in R language. For the older testR version written in Python 3, please see the renamed testr-py repo
on github: https://github.com/allr/testr-py

# Installation
Even thought the development of the package started sometime ago it is still rather experimental and no available from CRAN release yet. However, that is one of the near future plans to have a stable version and release it through CRAN.

It can be installed easily using the `devtools` package:

```r
library(devtools)
install_github('allr/testr')
```

To use R code filtering `rcov` package is needed. That can be installed in the similar way:

```r
library(devtools)
install_github('allr/rcov')
```

Or download the sources and build manually. If you're running R on Windows, you need to install Rtools.

Capturing runtime information 
---------------------

The usage of different functions in the R programs are traced and recorded in a text file named `capture`. 

To decorate function for its calls and argument use `Decorate(<FuncName>)`. If function is not visible outside the namespace, supply the namespace name as the second argument `Decorate(<FuncName>, <NamespaceName>)`. For example,

```r
library(testr)
Decorate("any")
any(T,F,F)
any(F,F)
library(stats)
Decorate(".cbind.ts", "stats")
cbind(ts(1:10), ts(2:20))
```
Capture information will be stored in `capture` directory in current working directory.

The capture file consists of entries, each of which is a record of a function call made in the execution with the following fields and the 
function calls will be replayed in test cases if valid.

  - `func`: function name
  - `args`: list of deparsed argument objects

For example,
```r
Decorate('agrep')
example(agrep)
```

Will generate tracing information for call to `agrep` in folder `capture` in current working directory.

Generating Test Cases
-------------------

TestGen converts a given capture file into test case set in testR format. To do so, one needs to call `TestGen`:

```r
TestGen(<PATH_TO_CAPTURE_FILE>, <OUTPUT_DIR>)
```

Each run of TestGen will create a folder under `<OUTPUT_DIR>` named with the current date and time to store the test case set generated.
The set consists of files with name `tc_<FUNC_NAME>.r`, where each function has its tests put together in a single file. A file named 
`bad_arguments` will be placed beside the test files for logging the invalid entries of the capture file. An entry is invalid if the 
recorded argument can't be restored properly due to a variety of reasons. A typical senario is when a 
function takes environment as arguments. Under `<OUTPUT_DIR>`, a link symbol `last` is set to 
point to the latest generated test set folder. The overall directory structure of the test set would look like:

    <OUTPUT_DIR>/
        2015-04-17 13:42:43/
            bad_arguments
            tc_foo.r
            tc_bar.r
            ...

Simplified Test Case Generation
-------------------------------
TestR includes tools to capture function calls in R programs and automatically convert them into test cases. 

It is possible to capture run time information and generate test cases with calling `code_gen` or `src_gen`:

```r
library(testr)
# generated test cases will be in `gen` folder
code_gen({any(T,F,F); all(T,F,F)}, "gen", c('any', 'all')) 
writeLines("example(agrep)\nexample(abbreviate)", "tmp.R")
# generated test cases will be in `gen` folder
src_gen("tmp.R", "gen", c('agrep', 'abbreviate'))
```

Which will capture function calls from source code, generate test cases. 

The other possible scenario is to execute all the available code for particular package from `CRAN` or `BioConductor`.

For example following command, will download and install `A3` package from `CRAN` and generate test cases for lm.

```r
library(testr)
cran_gen("A3", "gen", c('lm'))
```

If the `functions` argument is not supplied, test cases will be generated for all the functions in the package namespace.

```r
library(testr)
cran_gen("A3", "gen", c('lm'))
```

Running Test Cases
--------------

The test cases are generated in the compatible format with the test harness of TestR. To run the test set under `<TC_DIR>`:

```r
run_tests(<TC_DIR>)
```

Filtering generated test cases
=========================

One way to assess the completeness of the test set is to measure the code coverage rate. TestR includes a coverage reporter which 
supports generating various forms of summary of C file coverage by processing the output of `gcov`. 
Moreover `testR` uses [rcov](https://github.com/allr/rcov) for reporting R code coverage. This section briefly explains
how to use the reporter and filtering of test cases based on coverage.

Instrument GNU-R with GCOV
--------------------------

To use GNU-R as the tested VM, one needs to first build it with `gcov` support. Following shows how to properly set up the variables 
during configuration for Linux Distributions (tested on Ubuntu and Mint):

    ./configure CFLAGS='-O0 -fprofile-arcs -ftest-coverage' LDFLAGS='-fprofile-arcs' 

Configure for Mac OS X:

    ./configure r_arch=x86_64 CC="gcc -arch x86_64 -std=gnu99" CXX="g++ -arch x86_64" OBJC="gcc -arch x86_64" F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CFLAGS='-O0 -fprofile-arcs -ftest-coverage' LDFLAGS='-fprofile-arcs'

Measure Code C Coverage
---------------------

To invoke the reporter, call the following function located in coverage.r. The required argument is the the top level directory that
contains the .gcda files. For details of the result report, see comments in coverarge.r.

    measure_gcov(<PATH_TO_GCDA_FILES>)

Filtering generated test cases
----------------

To filter generated test cases in a single file call

```r
filter_tcs(<TC_DIR>, <OUTPUT_DIR>, <TC_DB>, <R_HOME>, <SRC_DIR>)
```

However, test cases generator usually provides large files with all the tests combined. In that cases, tests can be split and filtered with

```r
process_tc(<TC_FILE>, <OUTPUT_DIR>, <TC_DB>, <R_HOME>, <SRC_DIR>)
```

Note that all test cases will be split into single files.

Where,
* `<TC_FILE>` is a generated file with test cases
* `<OUTPUT_DIR>` is a folder with results
* `<TC_DB>` is a database of test cases to be compared to
* `<R_HOME>` is a R VM compiled with `gcov` support
* `<SRC_DIR>` is a folder in R VM to measure coverage (by default `src/main')
