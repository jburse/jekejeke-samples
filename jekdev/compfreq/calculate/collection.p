/**
 * Prolog code for the calculate collection test cases.
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

:- use_module(library(experiment/sets)).
:- use_module(library(experiment/ordsets)).

/****************************************************************/
/* sets.p extras                                                */
/****************************************************************/

runner:ref(eq_contains, 2, calculate_collection, 'XLOG 3.4.1').
runner:case(eq_contains, 2, calculate_collection, 'XLOG 3.4.1, XLOG 1') :-
   eq_contains([1, 2, 3, 2], 2).
runner:case(eq_contains, 2, calculate_collection, 'XLOG 3.4.1, XLOG 2') :-
   \+ eq_contains([1, 2, 3], 4).
runner:case(eq_contains, 2, calculate_collection, 'XLOG 3.4.1, XLOG 3') :-
   catch(eq_contains([1, 2|_], 4), error(E, _), true),
   E == instantiation_error.

runner:ref(eq_delete, 3, calculate_collection, 'XLOG 3.4.2').
runner:case(eq_delete, 3, calculate_collection, 'XLOG 3.4.2, XLOG 1') :-
   eq_delete([1, 2, 3, 2], 2, X),
   X == [1, 3].
runner:case(eq_delete, 3, calculate_collection, 'XLOG 3.4.2, XLOG 2') :-
   eq_delete([1, 2, 3], 4, X),
   X == [1, 2, 3].
runner:case(eq_delete, 3, calculate_collection, 'XLOG 3.4.2, XLOG 3') :-
   catch(eq_delete([1, 2|foo], 4, _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(eq_add, 3, calculate_collection, 'XLOG 3.4.3').
runner:case(eq_add, 3, calculate_collection, 'XLOG 3.4.3, XLOG 1') :-
   eq_add([1, 2, 3, 2], 2, X),
   X == [1, 2, 3, 2].
runner:case(eq_add, 3, calculate_collection, 'XLOG 3.4.3, XLOG 2') :-
   eq_add([1, 2, 3], 4, X),
   X == [4, 1, 2, 3].
runner:case(eq_add, 3, calculate_collection, 'XLOG 3.4.3, XLOG 3') :-
   catch(eq_add([1, 2|_], 4, _), error(E, _), true),
   E == instantiation_error.

runner:ref(eq_subtract, 3, calculate_collection, 'XLOG 3.4.4').
runner:case(eq_subtract, 3, calculate_collection, 'XLOG 3.4.4, XLOG 1') :-
   eq_subtract([1, 2, 3], [2, 3, 4], X),
   X == [1].
runner:case(eq_subtract, 3, calculate_collection, 'XLOG 3.4.4, XLOG 2') :-
   eq_subtract([1, 3], [2, 4], X),
   X == [1, 3].
runner:case(eq_subtract, 3, calculate_collection, 'XLOG 3.4.4, XLOG 3') :-
   eq_subtract([A, B], [B, _], X),
   X == [A].
runner:case(eq_subtract, 3, calculate_collection, 'XLOG 3.4.4, XLOG 4') :-
   catch(eq_subtract([1|foo], [2, 4], _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(eq_intersection, 3, calculate_collection, 'XLOG 3.4.5').
runner:case(eq_intersection, 3, calculate_collection, 'XLOG 3.4.5, XLOG 1') :-
   eq_intersection([1, 2, 3], [2, 3, 4], X),
   X == [2, 3].
runner:case(eq_intersection, 3, calculate_collection, 'XLOG 3.4.5, XLOG 2') :-
   eq_intersection([1, 3], [2, 4], X),
   X == [].
runner:case(eq_intersection, 3, calculate_collection, 'XLOG 3.4.5, XLOG 3') :-
   eq_intersection([_, B], [B, _], X),
   X == [B].
runner:case(eq_intersection, 3, calculate_collection, 'XLOG 3.4.5, XLOG 4') :-
   catch(eq_intersection([1|_], [2, 4], _), error(E, _), true),
   E == instantiation_error.

runner:ref(eq_union, 3, calculate_collection, 'XLOG 3.4.6').
runner:case(eq_union, 3, calculate_collection, 'XLOG 3.4.6, XLOG 1') :-
   eq_union([1, 2, 3], [2, 3, 4], X),
   X == [1, 2, 3, 4].
runner:case(eq_union, 3, calculate_collection, 'XLOG 3.4.6, XLOG 2') :-
   eq_union([1, 3], [2, 4], X),
   X == [1, 3, 2, 4].
runner:case(eq_union, 3, calculate_collection, 'XLOG 3.4.6, XLOG 3') :-
   eq_union([A, B], [B, C], X),
   X == [A, B, C].
runner:case(eq_union, 3, calculate_collection, 'XLOG 3.4.6, XLOG 4') :-
   catch(eq_union([1, 3], [2|foo], _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(eq_symdiff, 3, calculate_collection, 'XLOG 3.4.7').
runner:case(eq_symdiff, 3, calculate_collection, 'XLOG 3.4.7, XLOG 1') :-
   eq_symdiff([1, 2, 3], [2, 3, 4], X),
   X == [1, 4].
runner:case(eq_symdiff, 3, calculate_collection, 'XLOG 3.4.7, XLOG 2') :-
   eq_symdiff([1, 3], [2, 4], X),
   X == [1, 3, 2, 4].
runner:case(eq_symdiff, 3, calculate_collection, 'XLOG 3.4.7, XLOG 3') :-
   eq_symdiff([A, B], [B, C], X),
   X == [A, C].
runner:case(eq_symdiff, 3, calculate_collection, 'XLOG 3.4.7, XLOG 4') :-
   catch(eq_symdiff([1, 3], [2|_], _), error(E, _), true),
   E == instantiation_error.

runner:ref(eq_subset, 2, calculate_collection, 'XLOG 3.4.8').
runner:case(eq_subset, 2, calculate_collection, 'XLOG 3.4.8, XLOG 1') :-
   eq_subset([2, 3], [1, 2, 3, 4]).
runner:case(eq_subset, 2, calculate_collection, 'XLOG 3.4.8, XLOG 2') :-
   \+ eq_subset([1, 3], [2, 4]).
runner:case(eq_subset, 2, calculate_collection, 'XLOG 3.4.8, XLOG 3') :-
   eq_subset([B], [_, B, _]).
runner:case(eq_subset, 2, calculate_collection, 'XLOG 3.4.8, XLOG 4') :-
   catch(eq_subset([2|foo], [1, 2, 3, 4]), error(E, _), true),
   E == type_error(list, foo).

runner:ref(eq_disjoint, 2, calculate_collection, 'XLOG 3.4.9').
runner:case(eq_disjoint, 2, calculate_collection, 'XLOG 3.4.9, XLOG 1') :-
   \+ eq_disjoint([2, 3], [1, 2, 3, 4]).
runner:case(eq_disjoint, 2, calculate_collection, 'XLOG 3.4.9, XLOG 2') :-
   eq_disjoint([1, 3], [2, 4]).
runner:case(eq_disjoint, 2, calculate_collection, 'XLOG 3.4.9, XLOG 3') :-
   \+ eq_disjoint([B], [_, B, _]).
runner:case(eq_disjoint, 2, calculate_collection, 'XLOG 3.4.9, XLOG 4') :-
   catch(eq_disjoint([1, 3], [2|_]), error(E, _), true),
   E = instantiation_error.

runner:ref(eq_equal, 2, calculate_collection, 'XLOG 3.4.10').
runner:case(eq_equal, 2, calculate_collection, 'XLOG 3.4.10, XLOG 1') :-
   eq_equal([2, 3], [3, 2, 3]).
runner:case(eq_equal, 2, calculate_collection, 'XLOG 3.4.10, XLOG 2') :-
   \+ eq_equal([1, 3], [2, 4]).
runner:case(eq_equal, 2, calculate_collection, 'XLOG 3.4.10, XLOG 3') :-
   \+ eq_equal([B], [_, B, _]).
runner:case(eq_equal, 2, calculate_collection, 'XLOG 3.4.10, XLOG 4') :-
   catch(eq_equal([2, 3], [3, 2|foo]), error(E, _), true),
   E == type_error(list, foo).

/****************************************************************/
/* ordsets.p extras                                             */
/****************************************************************/

runner:ref(ord_contains, 2, calculate_collection, 'XLOG 3.5.1').
runner:case(ord_contains, 2, calculate_collection, 'XLOG 3.5.1, XLOG 1') :-
   ord_contains([1, 2, 3], 2).
runner:case(ord_contains, 2, calculate_collection, 'XLOG 3.5.1, XLOG 2') :-
   \+ ord_contains([1, 2, 4], 3).
runner:case(ord_contains, 2, calculate_collection, 'XLOG 3.5.1, XLOG 3') :-
   catch(ord_contains([1, 2|foo], 4), error(E, _), true),
   E = type_error(list, foo).

runner:ref(ord_delete, 3, calculate_collection, 'XLOG 3.5.2').
runner:case(ord_delete, 3, calculate_collection, 'XLOG 3.5.2, XLOG 1') :-
   ord_delete([1, 2, 3], 2, X),
   X == [1, 3].
runner:case(ord_delete, 3, calculate_collection, 'XLOG 3.5.2, XLOG 2') :-
   ord_delete([1, 2, 4], 3, X),
   X == [1, 2, 4].
runner:case(ord_delete, 3, calculate_collection, 'XLOG 3.5.2, XLOG 3') :-
   catch(ord_delete([1, 2|_], 4, _), error(E, _), true),
   E == instantiation_error.

runner:ref(ord_add, 3, calculate_collection, 'XLOG 3.5.3').
runner:case(ord_add, 3, calculate_collection, 'XLOG 3.5.3, XLOG 1') :-
   ord_add([1, 2, 3], 2, X),
   X == [1, 2, 3].
runner:case(ord_add, 3, calculate_collection, 'XLOG 3.5.3, XLOG 2') :-
   ord_add([1, 2, 4], 3, X),
   X == [1, 2, 3, 4].
runner:case(ord_add, 3, calculate_collection, 'XLOG 3.5.3, XLOG 3') :-
   catch(ord_add([1, 2|foo], 4, _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(ord_subtract, 3, calculate_collection, 'XLOG 3.5.4').
runner:case(ord_subtract, 3, calculate_collection, 'XLOG 3.5.4, XLOG 1') :-
   ord_subtract([1, 2, 3], [2, 3, 4], X),
   X == [1].
runner:case(ord_subtract, 3, calculate_collection, 'XLOG 3.5.4, XLOG 2') :-
   ord_subtract([1, 3], [2, 4], X),
   X == [1, 3].
runner:case(ord_subtract, 3, calculate_collection, 'XLOG 3.5.4, XLOG 3') :-
   catch(ord_subtract([1|_], [2, 4], _), error(E, _), true),
   E == instantiation_error.

runner:ref(ord_intersection, 3, calculate_collection, 'XLOG 3.5.5').
runner:case(ord_intersection, 3, calculate_collection, 'XLOG 3.5.5, XLOG 1') :-
   ord_intersection([1, 2, 3], [2, 3, 4], X),
   X == [2, 3].
runner:case(ord_intersection, 3, calculate_collection, 'XLOG 3.5.5, XLOG 2') :-
   ord_intersection([1, 3], [2, 4], X),
   X == [].
runner:case(ord_intersection, 3, calculate_collection, 'XLOG 3.5.5, XLOG 3') :-
   catch(ord_intersection([1|foo], [2, 4], _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(ord_union, 3, calculate_collection, 'XLOG 3.5.6').
runner:case(ord_union, 3, calculate_collection, 'XLOG 3.5.6, XLOG 1') :-
   ord_union([1, 2, 3], [2, 3, 4], X),
   X = [1, 2, 3, 4].
runner:case(ord_union, 3, calculate_collection, 'XLOG 3.5.6, XLOG 2') :-
   ord_union([1, 3], [2, 4], X),
   X = [1, 2, 3, 4].
runner:case(ord_union, 3, calculate_collection, 'XLOG 3.5.6, XLOG 3') :-
   catch(ord_union([1, 3], [2|_], _), error(E, _), true),
   E == instantiation_error.

runner:ref(ord_symdiff, 3, calculate_collection, 'XLOG 3.5.7').
runner:case(ord_symdiff, 3, calculate_collection, 'XLOG 3.5.7, XLOG 1') :-
   ord_symdiff([1, 2, 3], [2, 3, 4], X),
   X == [1, 4].
runner:case(ord_symdiff, 3, calculate_collection, 'XLOG 3.5.7, XLOG 2') :-
   ord_symdiff([1, 3], [2, 4], X),
   X == [1, 2, 3, 4].
runner:case(ord_symdiff, 3, calculate_collection, 'XLOG 3.5.7, XLOG 3') :-
   catch(ord_symdiff([1, 3], [2|foo], _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(ord_subset, 2, calculate_collection, 'XLOG 3.5.8').
runner:case(ord_subset, 2, calculate_collection, 'XLOG 3.5.8, XLOG 1') :-
   ord_subset([2, 3], [1, 2, 3, 4]).
runner:case(ord_subset, 2, calculate_collection, 'XLOG 3.5.8, XLOG 2') :-
   \+ ord_subset([1, 3], [2, 4]).
runner:case(ord_subset, 2, calculate_collection, 'XLOG 3.5.8, XLOG 3') :-
   catch(ord_subset([2|_], [1, 2, 3, 4]), error(E, _), true),
   E == instantiation_error.

runner:ref(ord_disjoint, 2, calculate_collection, 'XLOG 3.5.9').
runner:case(ord_disjoint, 2, calculate_collection, 'XLOG 3.5.9, XLOG 1') :-
   \+ ord_disjoint([2, 3], [1, 2, 3, 4]).
runner:case(ord_disjoint, 2, calculate_collection, 'XLOG 3.5.9, XLOG 2') :-
   ord_disjoint([1, 3], [2, 4]).
runner:case(ord_disjoint, 2, calculate_collection, 'XLOG 3.5.9, XLOG 3') :-
   catch(ord_disjoint([1, 3], [2|foo]), error(E, _), true),
   E == type_error(list, foo).
