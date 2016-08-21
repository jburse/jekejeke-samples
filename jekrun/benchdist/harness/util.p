/**
 * The Prolog text for the testing utilities.
 *
 * Copyright 2010-2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

for(_).
for(N) :- N > 1, M is N - 1, for(M).

:- meta_predicate test(?,0).
test(N, X) :- for(N), call(X), fail.
test(_, _).

show(T, G) :-
   write('\tin '),
   write(T),
   write('\t('),
   write(G),
   write(' gc) ms'), nl.

:- meta_predicate bench(?,0,?,?).
bench(M, X, T, G) :-
   uptime(T1),
   gctime(G1),
   test(M, X),
   uptime(T2),
   gctime(G2),
   T is T2 - T1,
   G is G2 - G1,
   write(X),
   show(T, G).

dummy.