/**
 * Prolog code for the calculate function test cases.
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

:- use_module(library(experiment/maps)).
:- use_module(library(experiment/ordmaps)).

/****************************************************************/
/* maps.p extras                                                */
/****************************************************************/

runner:ref(eq_get, 3, calculate_function, 'XLOG 3.6.1').
runner:case(eq_get, 3, calculate_function, 'XLOG 3.6.1, XLOG 1') :-
   eq_get([3-a, 1-b, 2-c], 1, X),
   X == b.
runner:case(eq_get, 3, calculate_function, 'XLOG 3.6.1, XLOG 2') :-
   \+ eq_get([3-a, 1-b, 2-c], 4, _).
runner:case(eq_get, 3, calculate_function, 'XLOG 3.6.1, XLOG 3') :-
   catch(eq_get([3-a, 1-b|_], 4, _), error(E, _), true),
   E == instantiation_error.

runner:ref(eq_put, 4, calculate_function, 'XLOG 3.6.2').
runner:case(eq_put, 4, calculate_function, 'XLOG 3.6.2, XLOG 1') :-
   eq_put([3-a, 1-b, 2-c], 1, d, X),
   X == [3-a, 1-d, 2-c].
runner:case(eq_put, 4, calculate_function, 'XLOG 3.6.2, XLOG 2') :-
   eq_put([3-a, 1-b, 2-c], 4, d, X),
   X == [3-a, 1-b, 2-c, 4-d].
runner:case(eq_put, 4, calculate_function, 'XLOG 3.6.2, XLOG 3') :-
   catch(eq_put([3-a, 1-b|foo], 4, d, _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(eq_remove, 3, calculate_function, 'XLOG 3.6.3').
runner:case(eq_remove, 3, calculate_function, 'XLOG 3.6.3, XLOG 1') :-
   eq_remove([3-a, 1-b, 2-c], 1, X),
   X == [3-a, 2-c].
runner:case(eq_remove, 3, calculate_function, 'XLOG 3.6.3, XLOG 2') :-
   eq_remove([3-a, 1-b, 2-c], 4, X),
   X = [3-a, 1-b, 2-c].
runner:case(eq_remove, 3, calculate_function, 'XLOG 3.6.3, XLOG 3') :-
   catch(eq_remove([3-a, 1-b|_], 4, _), error(E, _), true),
   E == instantiation_error.

/****************************************************************/
/* ordmaps.p extras                                             */
/****************************************************************/

runner:ref(ord_get, 3, calculate_function, 'XLOG 3.7.1').
runner:case(ord_get, 3, calculate_function, 'XLOG 3.7.1, XLOG 1') :-
   ord_get([1-a, 2-b, 3-c], 2, X),
   X == b.
runner:case(ord_get, 3, calculate_function, 'XLOG 3.7.1, XLOG 2') :-
   \+ ord_get([1-a, 2-b, 4-c], 3, _).

runner:ref(ord_put, 4, calculate_function, 'XLOG 3.7.2').
runner:case(ord_put, 4, calculate_function, 'XLOG 3.7.2, XLOG 1') :-
   ord_put([1-a, 2-b, 3-c], 2, d, X),
   X == [1-a, 2-d, 3-c].
runner:case(ord_put, 4, calculate_function, 'XLOG 3.7.2, XLOG 2') :-
   ord_put([1-a, 2-b, 4-c], 3, d, X),
   X == [1-a, 2-b, 3-d, 4-c].

runner:ref(ord_remove, 3, calculate_function, 'XLOG 3.7.3').
runner:case(ord_remove, 3, calculate_function, 'XLOG 3.7.3, XLOG 1') :-
   ord_remove([1-a, 2-b, 3-c], 2, X),
   X == [1-a, 3-c].
runner:case(ord_remove, 3, calculate_function, 'XLOG 3.7.3, XLOG 2') :-
   ord_remove([1-a, 2-b, 4-c], 3, X),
   X == [1-a, 2-b, 4-c].
