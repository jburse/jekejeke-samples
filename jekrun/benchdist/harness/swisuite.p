/**
 * The Prolog text that defines the test suite.
 *
 * Copyright 2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 1.0.3 (a fast and small prolog interpreter)
 */

:- ensure_loaded(util).

:- ensure_loaded('../tests/swicollatz').
:- ensure_loaded('../tests/swiqueens3').

suite :-
   bench(3001, dummy, _, _),
   bench(8, many, T1, G1),
   bench(8, many2, T2, G2),
   bench(8, many4, T3, G3),
   bench(8, many8, T4, G4),
   bench(12, first, T5, G5),
   bench(12, first2, T6, G6),
   bench(12, first4, T7, G7),
   bench(12, first8, T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7+T8,
   G is G1+G2+G3+G4+G5+G6+G7+G8,
   write('Total'),
   show(T, G), nl.

/*
suite2 :-
   bench(3001, dummy, _, _),
   bench(17, queens, T1, G1),
   bench(17, queens2, T2, G2),
   bench(17, queens4, T3, G3),
   bench(17, queens8, T4, G4),
   bench(17, setup, T5, G5),
   bench(17, setup2, T6, G6),
   bench(17, setup4, T7, G7),
   bench(17, setup8, T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7+T8,
   G is G1+G2+G3+G4+G5+G6+G7+G8,
   write('Total'),
   show(T, G), nl.
*/
suite2 :-
   bench(3001, dummy, _, _),
   bench(133, queens, T1, G1),
   bench(133, queens2, T2, G2),
   bench(133, queens4, T3, G3),
   bench(133, queens8, T4, G4),
   bench(133, setup, T5, G5),
   bench(133, setup2, T6, G6),
   bench(133, setup4, T7, G7),
   bench(133, setup8, T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7+T8,
   G is G1+G2+G3+G4+G5+G6+G7+G8,
   write('Total'),
   show(T, G), nl.