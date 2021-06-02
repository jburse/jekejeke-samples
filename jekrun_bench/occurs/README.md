# What is this?

This directory contains test cases for the occurs_check
Prolog flag. We test some side effect Prolog problems.
We don't test assert or file I/O.

After loading the files use the following predicates to run the test cases.
- **suite:** Benchmark for Occurs Check.

# Harness Entries

To use the a Prolog variant load the following file:
- [harness/jekejeke.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchoccurs/harness/jekejeke.p):
  Jekejeke Prolog main file.
- [harness/swi.p](http://github.com/jburse/jekejeke-samples/blob/master/jekrun/benchoccurs/harness/swi.p):
  SWI-Prolog main file.

In the Jekejeke Prolog interpreter you can directly point
an URL to the raw file, no need to download the
GitHub repository.

# Results Gallery

To see some results on our machines look at this directory:
- [galery](https://github.com/jburse/jekejeke-samples/tree/master/jekrun/benchoccurs/galery):
  Results Gallery

We compare our Prolog system with some other Prolog systems on
some hardware platforms.

# Issues and Contributing

Feel free to fork for non-commercial attributed use. There
is no warranty for the samples. You might try a pull
request if you find issues, but there is also no service
agreement by us. Note also that Jekejeke is a trademark.

Jan Burse, 04.03.2021