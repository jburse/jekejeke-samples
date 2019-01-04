/**
 * The Prolog text that defines the test suite.
 * Version that uses include instead of ensure_loaded.
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

:- include('gnuutil.p').

:- include('../tests/nrev.p').
:- include('../tests/crypt.p').
:- include('../tests/deriv.p').
:- include('../tests/poly.p').
:- include('../tests/qsort.p').
:- include('../tests/tictac.p').
:- include('../tests/queens.p').
:- include('../tests/query.p').
:- include('../tests/mtak.p').
:- include('../tests/perfect.p').
:- include('../tests/calc.p').

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

suite :-
   bench(3001, dummy, _, _),
   bench(6001, nrev, T1, G1),
   bench(301, crypt, T2, G2),
   bench(30001, deriv, T3, G3),
   bench(61, poly, T4, G4),
   bench(6001, sortq, T5, G5),
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