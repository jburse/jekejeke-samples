# What is this?

This directory contains test cases for the Prolog ISO core
functionality. We test some pure Prolog problems. We don't
test assert or file I/O.

# Jekejeke Prolog Variant

To use the Jekejeke Prolog variant load the following file:
- [harness/jekejeke.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchmark/harness/jekejeke.p):
  Jekejeke Prolog main file.

You can directly point the Jekejeke Prolog interpreter to the raw file,
no need to download the GitHub repository.

After loading the files use the following predicates to run the test
cases. For slower devices such as Android use the r-predicates:
- **suite:** Benchmark for ISO Core Prolog (normal iteration).
- **rsuite2:** Benchmark for ISO Core Prolog (reduced iteration).

# SWI-Prolog Variant

To use the SWI-Prolog variant load the following file:
- [harness/swi.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchmark/harness/swi.p)):
  SWI-Prolog main file.

After loading the files use the following predicates to run
the test cases:
- **suite:** Benchmark for ISO Core Prolog (normal iteration).

# Results Gallery

To see some results on our machines look at this directory:
- [galery](https://github.com/jburse/jekejeke-samples/tree/master/jekrun/benchmark/galery):
  Results Gallery

Our machines so far include a Lenovo Ideapad 700, a Lenovo Carbon X1
and a Sony Experia Tablet. On the non-Android devices we tested Jekejeke
Prolog and SWI-Prolog. On the Android device we tested Jekejeke Prolog.

# Issues and Contributing

Feel free to fork for non-commercial attributed use. There
is no warranty for the samples. You might try a pull
request if you find issues, but there is also no service
agreement by us. Note also that Jekejeke is a trademark.

Jan Burse, 21.08.2016