/**
 * SWI Prolog code for the test harness.
 *
 * Copyright 2011-2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

% ?- ensure_loaded('/Projects/Jekejeke/Prototyping/samples/jekrun/benchmark/harness/swi.p').

uptime(T) :-
   statistics(walltime, [T|_]).

gctime(T) :-
   statistics(garbage_collection, [_,_,T|_]).

:- set_prolog_flag(double_quotes, codes).
user:prolog_file_type(p, prolog).

:- ensure_loaded(swisuite).