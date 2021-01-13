/**
 * Prolog code for the calculate safe test cases.
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

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(basic/lists)).

/****************************************************************/
/* lists.p II                                                   */
/****************************************************************/

runner:ref(intersection, 3, calculate_safe, 'XLOG 3.9.1').
runner:case(intersection, 3, calculate_safe, 'XLOG 3.9.1, XLOG 1') :-
   intersection([1, 2, 3], [2, 3, 4], X),
   X == [2, 3].
runner:case(intersection, 3, calculate_safe, 'XLOG 3.9.1, XLOG 2') :-
   intersection([1, 3], [2, 4], X),
   X == [].
runner:case(intersection, 3, calculate_safe, 'XLOG 3.9.1, XLOG 3') :-
   catch(intersection([1|_], [2, 4], _), error(E, _), true),
   E == instantiation_error.

runner:ref(union, 3, calculate_safe, 'XLOG 3.9.2').
runner:case(union, 3, calculate_safe, 'XLOG 3.9.2, XLOG 1') :-
   union([1, 2, 3], [2, 3, 4], X),
   X == [1, 2, 3, 4].
runner:case(union, 3, calculate_safe, 'XLOG 3.9.2, XLOG 2') :-
   union([1, 3], [2, 4], X),
   X == [1, 3, 2, 4].
runner:case(union, 3, calculate_safe, 'XLOG 3.9.2, XLOG 3') :-
   catch(union([1|foo], [2, 4], _), error(E, _), true),
   E == type_error(list, foo).

