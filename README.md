*** This is the testR reimplementation in R language. For the older testR version written in Python 3, please see the renamed testr-py repo
*** on github:
***
*** https://github.com/allr/testr-py

testR
=====

TestR implementation in R. 


Instrument GNU-R with GCOV
==========================

In the process of building gnu-r, inform gcc to instrument the executable with gcov support:

./configure CFLAGS='-O0 -fprofile-arcs -ftest-coverage' LDFLAGS='-fprofile-arcs' 

After the build is done, for every object file, a .gcno will be generated beside for storing the 
source code structural information. Any C file not accompanied by a .gcno file is excluded
in the compilation, and therefore will be excluded as well in the coverage report.