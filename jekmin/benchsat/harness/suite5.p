/**
 * Test suite that tests CLP(FD).
 *
 * Copyright 2019, XLOG Technologies GmbH, Switzerland
 * Jekejeke Minlog 1.0.8 (minimal logic extension module)
 */

:- ensure_loaded('util.p').

:- ensure_loaded('../tests/pigeon.p').
:- ensure_loaded('../tests/ortho.p').
:- ensure_loaded('../tests/queens.p').
:- ensure_loaded('../tests/hilbert.p').
:- ensure_loaded('../tests/primes.p').
:- ensure_loaded('../tests/magic.p').
:- ensure_loaded('../tests/kitchen.p').

suite5 :-
   bench(1001, dummy, _, _),
   bench(28, pigeon(_), T1, G1),
   bench(53, ortho(_), T2, G2),
   bench(5, queens(_), T3, G3),
   bench(8, hilbert(_), T4, G4),
   numlen(10),
   bench(5, primes(_), T5, G5),
   numlen(4),
   bench(25, magic(_), T6, G6),
   bench(6, kitchen(_), T7, G7),
   T is T1+T2+T3+T4+T5+T6+T7,
   G is G1+G2+G3+G4+G5+G6+G7,
   write('Total'),
   show(T, G), nl.
