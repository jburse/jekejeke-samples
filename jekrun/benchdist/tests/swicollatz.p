/**
 * Balanced execution of the collatz function.
 *
 * Copyright 2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 1.1.6 (a fast and small prolog interpreter)
 */

:- ensure_loaded('../compat/swidistributed').

many :-
   between(10000, 20000, X), collatz(X, _).

many2 :-
   balance(between(10000, 20000, X), collatz(X, _), 2).

many4 :-
   balance(between(10000, 20000, X), collatz(X, _), 4).

many8 :-
   balance(between(10000, 20000, X), collatz(X, _), 8).

first :-
   once((between(10000, 20000, X), collatz(X, _), X = 16666)).

first2 :-
   once(balance(between(10000, 20000, X), (collatz(X, _), X = 16666), 2)).

first4 :-
   once(balance(between(10000, 20000, X), (collatz(X, _), X = 16666), 4)).

first8 :-
   once(balance(between(10000, 20000, X), (collatz(X, _), X = 16666), 8)).

collatz(1, 0) :- !.
collatz(I, N) :- 1 =:= I/\1, !,
   I0 is I*3+1, collatz(I0, N0), N is N0+1.
collatz(I, N) :-
   I0 is I>>1,  collatz(I0, N0), N is N0+1.