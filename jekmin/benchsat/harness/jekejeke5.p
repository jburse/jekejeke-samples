/**
 * Jekejeke Prolog code for the benchmark harness.
 *
 * Copyright 2013-2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Minlog 0.6.6 (minimal logic extension module)
 */

% ?- ensure_loaded('/Projects/Jekejeke/Prototyping/samples/jekmin/benchmark/jekejeke4.p').

% :- sys_add_path('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/').

uptime(X) :-
   statistics(uptime, X).

gctime(X) :-
   statistics(gctime, X).

:- ensure_loaded('suite5.p').
