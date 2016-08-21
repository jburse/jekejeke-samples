# What is this?

This directory contains test case for the new module "distributed".
This module provides work distribution predicates balance/3 and
setup_balance/3. We test these predicates for a collatz computation
and 8-queens problem.

There is also a small compatibility library for SWI-Prolog that
re-implememts the module "distribued" so that the same test
cases can be run on SWI-Prolog as well.

After loading the files use the following predicates to run
the test cases:
- suite: Collatz Computation.
- suite2: 8-Queens Problem.

# Jekejeke Prolog Variant

To use the Jekejeke Prolog variant load the following file:
- [jekejeke.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchdist/harness/jekejeke.p):
  Jekejeke Prolog main file.

You can directly point the Jekejeke Prolog interpreter to the raw file,
no need to download the GitHub repository.

# SWI-Prolog Variant

To use the SWI-Prolog variant load the following file:
- [swi.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchdist/harness/swi.p):
  SWI-Prolog main file.

# Issues and Contributing

Feel free to fork for non-commercial attributed use. There
are is no warranty for the samples, you might try a pull
request if you find issues, but there is also no service
agreement. Note also that Jekejeke is a trademark.

Jan Burse, 21.08.2016