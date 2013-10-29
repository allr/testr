*** This is the testR reimplementation in R language. For the older testR version written in Python 3, please see the renamed testr-py repo
*** on github:
***
*** https://github.com/allr/testr-py

testR
=====

TestR implementation in R. 


Automatic Test Case Generation
==============================

TestR includes tools to capture function calls in R programs and automatically convert them into test cases. Currently, only builtin functions
will be captured because they don't have unevaluated promises in their arguments while the bodys are being executed according to the R 
specification. Unevaluated promises needs to be handled specially, which will be dealt with in future work.

This section explains how to generate the test case set given a set of R programs.


Generate Capture File
---------------------

The usage of functions in the R programs are traced and recorded in a text file named `capture`. To generate the capture file, one needs to the
programs on the instrumented version of GNU-R, called `r-instrumented`, which is part of the `allr` project.

Checkout `r-instrumented` on branch `capture` by typing

    git clone -b capture https://github.com/allr/r-instrumented <R_INSTRUMENTED_HOME>

To trace the prgram `<R_PROG>`, type

    <R_INSTRUMENTED_HOME>/bin/R --no-restore --no-save --slave --trace capture --tracedir <TRACE_DIR> -f <R_PROG>

After the run, the traced information will be appended to file `<TRACE_DIR>/capture`.

The capture file consists of entries, each of which is a record of a function call made in the execution with the following fields and the 
function calls will be replayed in test cases if valid.

  - func: function name
  - type: `P` | `I`, indicating if the function is primitive or internal.
  - args: list of deparsed argument objects
  - retn: deparsed returned objects


Generate Test Cases
-------------------

TestGen converts a given capture file into test case set. To do so, one needs to call the following function located in `testgen.r`:

    testgen(<PATH_TO_CAPTURE_FILE>, <OUTPUT_DIR>)

Each run of TestGen will create a folder under `<OUTPUT_DIR>` named with the current date and time to store the test case set generated.
The set consists of files with name `tc_<FUNC_NAME>.r`, where each function has its tests put together in a single file. A file named 
`bad_arguments` will be placed beside the test files for logging the invalid entries of the capture file. An entry is invalid if the 
recorded argument and/or return values/objects cannot be restored properly due to a variety of reasons. A typical senario is when a 
function takes environment as arguments. For details, see comments in testgen.r. Under `<OUTPUT_DIR>`, a link symbol `last` is set to 
point to the latest generated test set folder. The overall directory structure of the test set would look like:

    <OUTPUT_DIR>/
      last
		    2013-10-17 13:42:43/
		    bad_arguments
        tc_foo.r
        tc_bar.r
			  ...


Run Test Cases
--------------

The test cases are generated in the compatible format with the test harness of TestR. To run the test set under `<TC_DIR>`, call the
following function in `target.r`:

     runTests(<TC_DIR>)


Code Coverage Measurement
=========================

One way to assess the completeness of the test set is to measure the code coverage rate. TestR includes a coverage reporter which 
supports generating various forms of summary of C file coverage by processing the output of `gcov`. This section briefly explains
how to use the reporter.


Instrument GNU-R with GCOV
--------------------------

To use GNU-R as the tested VM, one needs to first build it with `gcov` support. Following shows how to properly set up the variables 
during configuration:

    ./configure CFLAGS='-O0 -fprofile-arcs -ftest-coverage' LDFLAGS='-fprofile-arcs' 

After the build is done, for every .o file, a corresponding .gcno file will be generated for storing the source code structural 
information. Any C file not accompanied by a .gcno file was excluded in the compilation, and therefore will be excluded as well in 
the coverage report. `gcov` stores the coverage information in .gcda files and the coverage reporter relies on them to compute the
related numbers, therefore you may want to verify the existence of .gcda files before calling the reporter. For details about `gcov`, 
look at http://gcc.gnu.org/onlinedocs/gcc/index.html#toc_Gcov.


Measure Code Coverage
---------------------

To invoke the reporter, call the following function located in coverage.r. The required argument is the the top level directory that
contains the .gcda files. For details of the result report, see comments in coverarge.r.

    coverage(<PATH_TO_GCDA_FILES>)