# What is this?

This directory contains test cases for the Prolog ISO core
functionality. We test some pure Prolog problems. We don't
test assert or file I/O.

After loading the files use the following predicates to run the test
cases. For slower devices such as Android use the r-predicates:
- **suite:** Benchmark for ISO Core Prolog (normal iteration).
- **rsuite:** Benchmark for ISO Core Prolog (reduced iteration).

# Harness Entries

To use the a Prolog variant load the following file:
- [harness/jekejeke.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchmark/harness/jekejeke.p):
  Jekejeke Prolog main file.
- [harness/swi.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchmark/harness/swi.p):
  SWI-Prolog main file.
- [harness/gprolog.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchmark/harness/gprolog.p):
  GNU-Prolog main file.
- [harness/eclipse.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchmark/harness/eclipse.p):
  ECLiPSe CLP main file.

In the Jekejeke Prolog interpreter you can directly point
an URL to the raw file, no need to download the
GitHub repository.

# Results Gallery

To see some results on our machines look at this directory:
- [galery](https://github.com/jburse/jekejeke-samples/tree/master/jekrun/benchmark/galery):
  Results Gallery

Our machines so far include a Lenovo Ideapad 700 (4 Cores/8
Hyperthreads) and a Sony Experia Tablet (4 Cores). On the Android
device we tested only Jekejeke Prolog.

# Issues and Contributing

Feel free to fork for non-commercial attributed use. There
is no warranty for the samples. You might try a pull
request if you find issues, but there is also no service
agreement by us. Note also that Jekejeke is a trademark.

Jan Burse, 21.08.2016 (Updated 23.08.2016)