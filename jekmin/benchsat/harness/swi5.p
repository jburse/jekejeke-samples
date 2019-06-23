/**
 * SWI Prolog code for the benchmark harness.
 *
 * Copyright 2013-2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Minlog 0.6.6 (minimal logic extension module)
 */

% ?- ensure_loaded('/Projects/Jekejeke/Prototyping/samples/jekmin/benchmark/harness/swi4.p').

:- use_module(library(clpb)).

uptime(T) :-
   statistics(walltime, [T|_]).

gctime(T) :-
   statistics(garbage_collection, [_,_,T|_]).

count(L, N) :-
   sat_count(+[1|L], N).

-(X, Y) :-
   Y is -X.

:- ensure_loaded('suiteswi5.p').
