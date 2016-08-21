/**
 * The Prolog text that defines the test suite.
 *
 * Copyright 2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 1.0.3 (a fast and small prolog interpreter)
 */

:- ensure_loaded(util).

:- ensure_loaded('../tests/nrev').
:- ensure_loaded('../tests/crypt').
:- ensure_loaded('../tests/deriv').
:- ensure_loaded('../tests/poly').
:- ensure_loaded('../tests/qsort').
:- ensure_loaded('../tests/tictac').
:- ensure_loaded('../tests/queens').
:- ensure_loaded('../tests/query').
:- ensure_loaded('../tests/mtak').
:- ensure_loaded('../tests/perfect').
:- ensure_loaded('../tests/calc').

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

suite :-
   bench(3001, dummy, _, _),
   bench(6001, nrev, T1, G1),
   bench(301, crypt, T2, G2),
   bench(30001, deriv, T3, G3),
   bench(61, poly, T4, G4),
   bench(6001, qsort, T5, G5),
   bench(11, tictac, T6, G6),
   bench(16, queens, T7, G7),
   bench(3001, query, T8, G8),
   bench(31, mtak, T9, G9),
   bench(16, perfect, T10, G10),
   bench(20001, calc, T11, G11),
   T is T1+T2+T3+T4+T5+T6+T7+T8+T9+T10+T11,
   G is G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11,
   write('Total'),
   show(T, G), nl.

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

rsuite :-
   bench(301, dummy, _, _),
   bench(1201, rnrev, T1, G1),
   bench(61, crypt, T2, G2),
   bench(6001, rderiv, T3, G3),
   bench(12, rpoly, T4, G4),
   bench(1201, rqsort, T5, G5),
   bench(1, tictac, T6, G6),
   bench(6, rqueens, T7, G7),
   bench(301, query, T8, G8),
   bench(3, mtak, T9, G9),
   bench(1, perfect, T10, G10),
   bench(2001, rcalc, T11, G11),
   T is T1+T2+T3+T4+T5+T6+T7+T8+T9+T10+T11,
   G is G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11,
   write('Total'),
   show(T, G), nl.