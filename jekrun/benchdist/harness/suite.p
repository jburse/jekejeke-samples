/**
 * The Prolog text that defines the test suite.
 * Jekejeke Prolog version.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- ensure_loaded(util).

:- ensure_loaded('../tests/collatz').
:- ensure_loaded('../tests/queens3').
:- ensure_loaded('../tests/pool').

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

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
   T is T1+T2+T3+T4+T5+T6+T7 + T8,
   G is G1+G2+G3+G4+G5+G6+G7 + G8,
   write('Total'),
   show(T, G), nl.

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
   T is T1+T2+T3+T4+T5+T6+T7 + T8,
   G is G1+G2+G3+G4+G5+G6+G7 + G8,
   write('Total'),
   show(T, G), nl.

suite3 :-
   bench(3001, dummy, _, _),
   bench(8, pool, T1, G1),
   bench(8, pool2, T2, G2),
   bench(8, pool4, T3, G3),
   bench(8, pool8, T4, G4),
   bench(12, gotcha, T5, G5),
   bench(12, gotcha2, T6, G6),
   bench(12, gotcha4, T7, G7),
   bench(12, gotcha8, T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7 + T8,
   G is G1+G2+G3+G4+G5+G6+G7 + G8,
   write('Total'),
   show(T, G), nl.

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

rsuite :-
   bench(301, dummy, _, _),
   bench(8, rmany, T1, G1),
   bench(8, rmany2, T2, G2),
   bench(8, rmany4, T3, G3),
   bench(8, rmany8, T4, G4),
   bench(12, rfirst, T5, G5),
   bench(12, rfirst2, T6, G6),
   bench(12, rfirst4, T7, G7),
   bench(12, rfirst8, T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7 + T8,
   G is G1+G2+G3+G4+G5+G6+G7 + G8,
   write('Total'),
   show(T, G), nl.

rsuite2 :-
   bench(301, dummy, _, _),
   bench(4, rqueens, T1, G1),
   bench(4, rqueens2, T2, G2),
   bench(4, rqueens4, T3, G3),
   bench(4, rqueens8, T4, G4),
   bench(4, rsetup, T5, G5),
   bench(4, rsetup2, T6, G6),
   bench(4, rsetup4, T7, G7),
   bench(4, rsetup8, T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7 + T8,
   G is G1+G2+G3+G4+G5+G6+G7 + G8,
   write('Total'),
   show(T, G), nl.

