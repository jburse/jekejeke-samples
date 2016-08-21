/**
 * Jekejeke Prolog code for the benchmark harness.
 *
 * Copyright 2010-2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

% ?- ensure_loaded('/Projects/Jekejeke/Prototyping/samples/jekrun/benchmark/jekejeke.p').

uptime(X) :-
   statistics(uptime, X).

gctime(X) :-
   statistics(gctime, X).

:- ensure_loaded(suite).
