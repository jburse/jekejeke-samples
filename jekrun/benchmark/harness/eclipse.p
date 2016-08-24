/**
 * ECLiPSe Constraint Logic Programming System code for the test harness.
 *
 * Copyright 2011-2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

% ?- ensure_loaded('//C/Projects/Jekejeke/Prototyping/samples/jekrun/benchmark/harness/eclipse.p').

uptime(X) :-
   statistics(times, [_,_,T]),
   X is round(T*1000).

gctime(X) :-
   statistics(gc_time, T),
   X is round(T*1000).

:- use_module(library(iso)).
:- op(1150, fx, meta_predicate).
:- get_flag(prolog_suffix, L), append(L, [`.p`], R), set_flag(prolog_suffix, R).

:- ensure_loaded(suite).