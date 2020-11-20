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

:- use_module(library(advanced/sets)).
:- use_module(library(advanced/ordsets)).

runner:ref(contains, 2, calculate_collection, 'XLOG 3.2.1').
runner:case(contains, 2, calculate_collection, 'XLOG 3.2.1, XLOG 1') :-
   contains([1, 2, 3, 2], 2).
runner:case(contains, 2, calculate_collection, 'XLOG 3.2.1, XLOG 1') :-
   \+ contains([1, 2, 3], 4).

runner:ref(delete, 3, calculate_collection, 'XLOG 3.2.2').
runner:case(delete, 3, calculate_collection, 'XLOG 3.2.2, XLOG 1') :-
   delete([1, 2, 3, 2], 2, X),
   X == [1, 3].
runner:case(delete, 3, calculate_collection, 'XLOG 3.2.2, XLOG 2') :-
   delete([1, 2, 3], 4, X),
   X == [1, 2, 3].

runner:ref(add, 3, calculate_collection, 'XLOG 3.2.3').
runner:case(add, 3, calculate_collection, 'XLOG 3.2.3, XLOG 1') :-
   add([1, 2, 3, 2], 2, X),
   X == [1, 2, 3, 2].
runner:case(add, 3, calculate_collection, 'XLOG 3.2.3, XLOG 2') :-
   add([1, 2, 3], 4, X),
   X == [4, 1, 2, 3].

runner:ref(subtract, 3, calculate_collection, 'XLOG 3.2.4').
runner:case(subtract, 3, calculate_collection, 'XLOG 3.2.4, XLOG 1') :-
   subtract([1, 2, 3], [2, 3, 4], X),
   X == [1].
runner:case(subtract, 3, calculate_collection, 'XLOG 3.2.4, XLOG 2') :-
   subtract([1, 3], [2, 4], X),
   X == [1, 3].

runner:ref(intersection, 3, calculate_collection, 'XLOG 3.2.5').
runner:case(intersection, 3, calculate_collection, 'XLOG 3.2.5, XLOG 1') :-
   intersection([1, 2, 3], [2, 3, 4], X),
   X == [2, 3].
runner:case(intersection, 3, calculate_collection, 'XLOG 3.2.5, XLOG 2') :-
   intersection([1, 3], [2, 4], X),
   X == [].

runner:ref(union, 3, calculate_collection, 'XLOG 3.2.6').
runner:case(union, 3, calculate_collection, 'XLOG 3.2.6, XLOG 1') :-
   union([1, 2, 3], [2, 3, 4], X),
   X == [1, 2, 3, 4].
runner:case(union, 3, calculate_collection, 'XLOG 3.2.6, XLOG 2') :-
   union([1, 3], [2, 4], X),
   X == [1, 3, 2, 4].

runner:ref(symdiff, 3, calculate_collection, 'XLOG 3.2.7').
runner:case(symdiff, 3, calculate_collection, 'XLOG 3.2.7, XLOG 1') :-
   symdiff([1, 2, 3], [2, 3, 4], X),
   X == [1, 4].
runner:case(symdiff, 3, calculate_collection, 'XLOG 3.2.7, XLOG 1') :-
   symdiff([1, 3], [2, 4], X),
   X == [1, 3, 2, 4].

runner:ref(subset, 2, calculate_collection, 'XLOG 3.2.8').
runner:case(subset, 2, calculate_collection, 'XLOG 3.2.8, XLOG 1') :-
   subset([2, 3], [1, 2, 3, 4]).
runner:case(subset, 2, calculate_collection, 'XLOG 3.2.8, XLOG 2') :-
   \+ subset([1, 3], [2, 4]).

runner:ref(disjoint, 2, calculate_collection, 'XLOG 3.2.9').
runner:case(disjoint, 2, calculate_collection, 'XLOG 3.2.9, XLOG 1') :-
   \+ disjoint([2, 3], [1, 2, 3, 4]).
runner:case(disjoint, 2, calculate_collection, 'XLOG 3.2.9, XLOG 2') :-
   disjoint([1, 3], [2, 4]).

runner:ref(equal, 2, calculate_collection, 'XLOG 3.2.10').
runner:case(equal, 2, calculate_collection, 'XLOG 3.2.10, XLOG 1') :-
   \+ equal([2, 3], [1, 2, 3, 4]).
runner:case(equal, 2, calculate_collection, 'XLOG 3.2.10, XLOG 2') :-
   \+ equal([1, 3], [2, 4]).
