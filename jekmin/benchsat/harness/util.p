/**
 * The Prolog text for the testing utilities.
 *
 * Copyright 2013-2019, XLOG Technologies GmbH, Switzerland
 * Jekejeke Minlog 0.6.6 (minimal logic extension module)
 */

/**
 * for(N):
 * The predicate succeeds N times.
 */
% for(+Integer)
for(_).
for(N) :-
   N > 1,
   M is N-1,
   for(M).

/**
 * test(N, G):
 * The predicate succeeds for N times executing the goal G.
 */
% test(+Integer, +Goal)
:- meta_predicate(test(?,0)).
test(N, X) :-
   for(N),
   call(X), fail.
test(_, _).

/**
 * show(T, G):
 * The predicate succeeds in writing the cpu time T and the gc time G.
 */
% show(+Integer, +Integer)
show(T, G) :-
   write('\tin '),
   write(T),
   write('\t('),
   write(G),
   write(' gc) ms'), nl.

/**
 * bench(M, X, T, G):
 * The predicates succeeds in measuring the cpu time T and the gc time G
 * for M times executing the goal X.
 */
% bench(+Integer, +Goal, -Integer, -Integer)
:- meta_predicate(bench(?,0,?,?)).
bench(M, X, T, G) :-
   uptime(T1),
   gctime(G1),
   test(M, X),
   uptime(T2),
   gctime(G2),
   T is T2-T1,
   G is G2-G1,
   sys_func(X, F, _),
   write(F),
   show(T, G).

/**
 * dummy:
 * The predicate succeeds.
 */
dummy.

/**
 * sys_func(C, F, A):
 * The predicate succeeds for the functor F and arity A of the callable C.
 */
% sys_func(+Callable, -Atom, -Integer):
sys_func(X, _, _) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_func(M:X, M:F, A) :- !,
   sys_func(X, F, A).
sys_func(X, F, A) :-
   functor(X, F, A).
