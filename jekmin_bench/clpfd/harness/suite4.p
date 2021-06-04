/**
 * Test suite that tests CLP(FD).
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- ensure_loaded('util.p').

:- ensure_loaded('../tests/grocery3.p').
:- ensure_loaded('../tests/pythago3.p').
:- ensure_loaded('../tests/queens3.p').
:- ensure_loaded('../tests/money3.p').
:- ensure_loaded('../tests/crypt3.p').
:- ensure_loaded('../tests/wolfram3.p').
:- ensure_loaded('../tests/zebra3.p').
:- ensure_loaded('../tests/pigeon3.p').

suite4 :-
   bench(1001, dummy, _, _),
   bench(1, grocery3(_), T1, G1),
   bench(11, pythago3(_), T2, G2),
   bench(17, queens3(_), T3, G3),
   bench(21, money3(_), T4, G4),
   bench(80, crypt3(_), T5, G5),
   bench(25, wolfram3(_), T6, G6),
   bench(273, zebra3(_), T7, G7),
   bench(10, pigeon3(_), T8, G8),
   T is T1+T2+T3+T4+T5+T6+T7+T8,
   G is G1+G2+G3+G4+G5+G6+G7+G8,
   write('Total'),
   show(T, G), nl.
