# What is this?

This directory contains test cases for the new module "distributed".
This module provides work distribution meta-predicates. We tested these
predicates for a Collatz computation and a CLP(FD) queens problem.

There is also a small compatibility library for SWI-Prolog that
re-implememts the module "distribued" so that the same test
cases can be run on SWI-Prolog as well. We see similar parallel
scaling in Jekejeke Prolog and SWI-Prolog.

# Jekejeke Prolog Variant

To use the Jekejeke Prolog variant load the following file:
- [harness/jekejeke.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchdist/harness/jekejeke.p):
  Jekejeke Prolog main file.

You can directly point the Jekejeke Prolog interpreter to the raw file,
no need to download the GitHub repository.

After loading the files use the following predicates to run the test
cases. For slower devices such as Android use the r-predicates:
- **suite:** Collatz Computation from 10000-20000 (8 resp. 12 times).
- **suite2:** CLP(FD) 8-Queens Problem (17 times).
- **rsuite:** Collatz Computation from 1000-2000 (8 resp. 12 times).
- **rsuite2:** CLP(FD) 6-Queens Problem (4 times).

# SWI-Prolog Variant

To use the SWI-Prolog variant load the following file:
- [harness/swi.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchdist/harness/swi.p):
  SWI-Prolog main file.

After loading the files use the following predicates to run
the test cases:
- **suite:** Collatz Computation from 10000-20000 (8 resp. 12 times).
- **suite2:** CLP(FD) 8-Queens Problem (133 times).

# Issues and Contributing

Feel free to fork for non-commercial attributed use. There
are is no warranty for the samples, you might try a pull
request if you find issues, but there is also no service
agreement. Note also that Jekejeke is a trademark.

Jan Burse, 21.08.2016