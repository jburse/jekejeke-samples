/**
 * Prolog code for the structure set theory test cases.
 *
 * Source of test cases are the following standards and proposals:
 *   - Prolog General Core ISO/IUEC 13211-1
 *   - Draft Technical Corrigendum 2, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2</a>
 *   - New built-in flags, predicates, and functions proposal
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/N208">www.complang.tuwien.ac.at/ulrich/iso-prolog/N208</a>
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
:- ensure_loaded('../harness/data').

:- use_module(library(advanced/arith)).

/****************************************************************/
/* Set Predicates                                               */
/****************************************************************/

/* findall(T, G, L) */

runner:ref(findall, 3, structure_set, 'ISO 8.10.1.4').
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 1') :-
   findall(X, (X = 1; X = 2), S),
   S == [1, 2].
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 2') :-
   findall(X+Y, X = 1, S),
   nonvar(S), S = [1+Z], Z \== Y.
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 3') :-
   findall(_, fail, S),
   S == [].
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 4') :-
   findall(X, (X = 1; X = 1), S),
   S == [1, 1].
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 5') :-
   \+ findall(X, (X = 2; X = 1), [1, 2]).
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 7') :-
   findall(X, (X = 1; X = 2), [X, Y]),
   X == 1, Y == 2.
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 8') :-
   catch(findall(_, _, _), error(E, _), true),
   E == instantiation_error.
runner:case(findall, 3, structure_set, 'ISO 8.10.1.4, ISO 9') :-
   catch(findall(_, 4, _), error(E, _), true),
   E == type_error(callable, 4).

/* sort(L, R) */

runner:ref(sort, 2, structure_set, 'Corr.2 8.4.3.4').
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 1') :-
   sort([1, 1.0], L), L == [1.0, 1].
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 2') :-
   sort([1.0, X, a, a, X], L), L == [X, 1.0, a].
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 3') :-
   sort([north(a), shorter, short, foo(a, b)], L),
   L == [short, shorter, north(a), foo(a, b)].
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, ISO 6') :-
   sort([f(U), V, f(V), U], L),
   (L == [U, V, f(U), f(V)]; L == [V, U, f(V), f(U)]).
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 4') :-
   findall(Y, a(_, Y), L), sort(L, R), L = [A, B], (R == [A, B]; R == [B, A]).
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 5') :-
   catch(sort(_, _), error(E, _), true),
   E == instantiation_error.
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 6') :-
   catch(sort([77|35], _), error(E, _), true),
   nonvar(E), E = type_error(list, _).
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 7') :-
   sort([a, 'A', b, 'B'], L),
   L == ['A', 'B', a, b].
runner:case(sort, 2, structure_set, 'Corr.2 8.4.3.4, XLOG 8') :-
   sort([ü, u, œ, o], L),
   L == [o, u, ü, œ].

/* keysort(L, R) */

runner:ref(keysort, 2, structure_set, 'Corr.2 8.4.4.4').
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 1') :-
   keysort([1-x, 1.0-y], L), L == [1.0-y, 1-x].
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 2') :-
   keysort([1.0-z, X-x, a-y, a-x, X-y], L),
   L == [X-x, X-y, 1.0-z, a-y, a-x].
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 3') :-
   keysort([north(a)-x, shorter-y, short-z, foo(a, b)-t], L),
   L == [short-z, shorter-y, north(a)-x, foo(a, b)-t].
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 4') :-
   keysort([f(U)-x, V-y, f(V)-z, U-t], L),
   (L == [U-t, V-y, f(U)-x, f(V)-z]; L == [V-y, U-t, f(V)-z, f(U)-x]).
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 5') :-
   findall(X-Y, a(X, Y), L), keysort(L, R), L == R.
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 6') :-
   catch(keysort(_, _), error(E, _), true),
   E == instantiation_error.
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 7') :-
   catch(keysort([77-x|35], _), error(E, _), true),
   nonvar(E), E = type_error(list, _).
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 8') :-
   catch(keysort([77], _), error(E, _), true),
   E == type_error(pair, 77).
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 9') :-
   keysort([a-1, 'A'-2, a-3, 'B'-4, b-5, 'B'-6], L),
   L == ['A'-2, 'B'-4, 'B'-6, a-1, a-3, b-5].
runner:case(keysort, 2, structure_set, 'Corr.2 8.4.4.4, XLOG 10') :-
   keysort([ü-1, u-2, ü-3, œ-4, o-5, o-6], L),
   L == [o-5, o-6, u-2, ü-1, ü-3, œ-4].

/* bagof(T, A1^...^An^G, L) */

runner:ref(bagof, 3, structure_set, 'ISO 8.10.2.4').
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 1') :-
   bagof(X, (X = 1; X = 2), S), S == [1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 2') :-
   bagof(X, (X = 1; X = 2), X), X == [1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 3') :-
   bagof(X, (X = Y; X = Z), S), S == [Y, Z].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 4') :-
   \+ bagof(_, fail, _).
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 5a') :-
   bagof(1, (Y = 1; Y = 2), L), Y == 1, L == [1].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 5b') :-
   bagof(1, (Y = 1; Y = 2), L), Y == 2, L == [1].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 6') :-
   bagof(f(X, Y), (X = a; Y = b), L), L = [f(a, A), f(B, b)],
   var(A), A \== X, A \== Y, var(B), B \== X, B \== Y, A \== B.
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 7') :-
   bagof(X, Y^(X = 1, Y = 1; X = 2, Y = 2), S), S == [1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 8') :-
   bagof(X, Y^((X = 1; Y = 1); X = 2, Y = 2), S), S = [1, A, 2], var(A), A \== X, A \== Y.
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 9') :-
   catch(bagof(X, (Y^(X = 1; Y = 2); X = 3), _), error(E, _), true),
   nonvar(E), E = existence_error(_, ^ /2).
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 10a') :-
   bagof(X, (X = Y; X = Z; Y = 1), L), var(Y), L == [Y, Z].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 10b') :-
   bagof(X, (X = Y; X = Z; Y = 1), L), Y == 1, L = [A], var(A), A \== X, A \== Z.
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 11') :-
   bagof(X, a(X, Y), L), Y = f(A), var(A), A \== X, A \== Y, L == [1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 12a') :-
   bagof(X, b(X, Y), L), Y == 1, L == [1, 1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 12b') :-
   bagof(X, b(X, Y), L), Y == 2, L == [1, 2, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 13') :-
   catch(bagof(_, _^_, _), error(E, _), true),
   E == instantiation_error.
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, ISO 14') :-
   catch(bagof(_, 1, _), error(E, _), true),
   E == type_error(callable, 1).

runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, XLOG 1a') :-
   findall(Y-S, bagof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S), [R|_]),
   R == 1-[3, 1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, XLOG 1b') :-
   findall(Y-S, bagof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S), [_, R|_]),
   R == 2-[3, 1, 2, 3, 1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, XLOG 1c') :-
   findall(Y-S, bagof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S), [_, _]).
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, XLOG 2a') :-
   findall((Y-S, A-B), bagof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S), [(R, A-B)|_]),
   R == B-[3, 1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, XLOG 2b') :-
   findall((Y-S, A-B), bagof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S), [_, (R, A-B)|_]),
   R == f(A)-[3, 1, 2, 3, 1, 2].
runner:case(bagof, 3, structure_set, 'ISO 8.10.2.4, XLOG 2c') :-
   findall((Y-S, A-B), bagof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S), [_, _]).

/* setof(T, A1^...^An^G, L) */

runner:ref(setof, 3, structure_set, 'ISO 8.10.2.4, Corrigendum 1').
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 1') :-
   setof(X, (X = 1; X = 2), S), S == [1, 2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 2') :-
   setof(X, (X = 1; X = 2), X), X == [1, 2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 3') :-
   setof(X, (X = 2; X = 1), S), S == [1, 2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 4') :-
   setof(X, (X = 2; X = 2), S), S == [2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 5') :-
   setof(X, (X = Y; X = Z), S), (S == [Y, Z]; S == [Z, Y]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 6') :-
   \+ setof(_, fail, _).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 7a') :-
   setof(1, (Y = 1; Y = 2), L), Y == 1, L == [1].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 7b') :-
   setof(1, (Y = 1; Y = 2), L), Y == 2, L == [1].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 8') :-
   setof(f(X, Y), (X = a; Y = b), L), L = [f(B, b), f(a, A)],
   var(A), A \== X, A \== Y, var(B), B \== X, B \== Y, A \== B.
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 9') :-
   setof(X, Y^(X = 1, Y = 1; X = 2, Y = 2), S), S == [1, 2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 10') :-
   setof(X, Y^((X = 1; Y = 1); X = 2, Y = 2), S), S = [A, 1, 2], var(A), A \== X, A \== Y.
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 11') :-
   catch(setof(X, (Y^(X = 1; Y = 2); X = 3), _), error(E, _), true),
   nonvar(E), E = existence_error(_, ^ /2).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 12a') :-
   setof(X, (X = Y; X = Z; Y = 1), L), var(Y), (L == [Y, Z]; L == [Z, Y]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 12b') :-
   setof(X, (X = Y; X = Z; Y = 1), L), Y == 1, L = [A], var(A), A \== X, A \== Z.
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 13') :-
   setof(X, a(X, Y), L), Y = f(A), var(A), A \== X, A \== Y, L == [1, 2].

runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 14') :-
   setof(X, member(X, [f(U, b), f(V, c)]), L), (L == [f(U, b), f(V, c)]; L == [f(V, c), f(U, b)]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 15a') :-
   \+ setof(X, member(X, [f(U, b), f(V, c)]), [f(a, c), f(a, b)])
;  \+ setof(X, member(X, [f(U, b), f(V, c)]), [f(a, b), f(a, c)]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 15b') :-
   setof(X, member(X, [f(U, b), f(V, c)]), [f(a, c), f(a, b)]), U == a, V == a
;  setof(X, member(X, [f(U, b), f(V, c)]), [f(a, b), f(a, c)]), U == a, V == a.
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 16') :-
   setof(X, member(X, [f(b, U), f(c, V)]), [f(b, a), f(c, a)]), U == a, V == a.
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 17') :-
   setof(X, member(X, [V, U, f(U), f(V)]), L), (L == [U, V, f(U), f(V)]; L == [V, U, f(V), f(U)]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 18') :-
   setof(X, member(X, [V, U, f(U), f(V)]), [a, b, f(a), f(b)]), (U == a, V == b; U == b, V == a).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 19') :-
   \+ setof(X, member(X, [V, U, f(U), f(V)]), [a, b, f(b), f(a)]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 20') :-
   setof(X, exists(U, V)^member(X, [V, U, f(U), f(V)]), [a, b, f(a), f(b)]).
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 21a') :-
   setof(X, b(X, Y), L), Y == 1, L == [1, 2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 21b') :-
   setof(X, b(X, Y), L), Y == 2, L == [1, 2].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 22') :-
   setof(X-Xs, Y^setof(Y, b(X, Y), Xs), L),
   L == [1-[1, 2], 2-[1, 2]].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 23') :-
   setof(X-Xs, setof(Y, b(X, Y), Xs), L),
   L == [1-[1, 2], 2-[1, 2]].
runner:case(setof, 3, structure_set, 'ISO 8.10.2.4, ISO 24') :-
   setof(X-Xs, bagof(Y, d(X, Y), Xs), L),
   L == [1-[1, 2, 1], 2-[2, 1, 2]].
