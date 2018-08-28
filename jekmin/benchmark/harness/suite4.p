/**
 * Test suite that tests CLP(FD).
 *
 * Copyright 2013-2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Minlog 0.6.6 (minimal logic extension module)
 */

:- ensure_loaded('util.p').

:- ensure_loaded('../tests/grocery3.p').
:- ensure_loaded('../tests/pythago3.p').
:- ensure_loaded('../tests/queens3.p').
:- ensure_loaded('../tests/money3.p').
:- ensure_loaded('../tests/crypt3.p').
:- ensure_loaded('../tests/zebra3.p').
:- ensure_loaded('../tests/pigeon3.p').

suite4 :-
   bench(1001, dummy, _, _),
   bench(1, grocery3(_), T1, G1),
   bench(11, pythago3(_), T2, G2),
   bench(17, queens3(_), T3, G3),
   bench(21, money3(_), T4, G4),
   bench(80, crypt3(_), T5, G5),
   bench(273, zebra3(_), T6, G6),
   bench(10, pigeon3(_), T7, G7),
   T is T1+T2+T3+T4+T5+T6+T7,
   G is G1+G2+G3+G4+G5+G6+G7,
   write('Total'),
   show(T, G), nl.
