/**
 * The Prolog text that defines the test suite.
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

:- ensure_loaded(util).

:- ensure_loaded('../tests/peano').
:- ensure_loaded('../tests/peano2').
:- ensure_loaded('../tests/aristoteles').
:- ensure_loaded('../tests/pelletier').

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

suite :-
   bench(3001, dummy, _, _),
   bench(120, peano, T1, G1),
   set_prolog_flag(occurs_check, true),
   bench(120, peano2, T2, G2),
   set_prolog_flag(occurs_check, false),
   bench(100, aristo, T3, G3),
   set_prolog_flag(occurs_check, true),
   bench(100, aristo2, T4, G4),
   set_prolog_flag(occurs_check, false),
   bench(4, pelle, T5, G5),
   set_prolog_flag(occurs_check, true),
   bench(4, pelle2, T6, G6),
   set_prolog_flag(occurs_check, false),
   T is T1+T2+T3+T4+T5+T6,
   G is G1+G2+G3+G4+G5+G6,
   write('Total'),
   show(T, G), nl.
