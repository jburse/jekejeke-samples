/**
 * Prolog code for the calculate stable test cases.
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
/* lists.p I                                                    */
/****************************************************************/

runner:ref(append, 3, calculate_stable, 'XLOG 3.8.1').
runner:case(append, 3, calculate_stable, 'XLOG 3.8.1, XLOG 1') :-
   append([a, b], [c], X),
   X == [a, b, c].
runner:case(append, 3, calculate_stable, 'XLOG 3.8.1, XLOG 2') :-
   append(X, [c], [a, b, c]),
   X == [a, b].
runner:case(append, 3, calculate_stable, 'XLOG 3.8.1, XLOG 3') :-
   append([a, b], X, [a, b, c]),
   X == [c].
runner:case(append, 3, calculate_stable, 'XLOG 3.8.1, XLOG 4') :-
   findall(X-Y, append(X, Y, [a, b, c]), L),
   L == [[]-[a, b, c], [a]-[b, c], [a, b]-[c], [a, b, c]-[]].

runner:ref(reverse, 2, calculate_stable, 'XLOG 3.8.2').
runner:case(reverse, 2, calculate_stable, 'XLOG 3.8.2, XLOG 1') :-
   reverse([a, b, c], X),
   X == [c, b, a].
runner:case(reverse, 2, calculate_stable, 'XLOG 3.8.2, XLOG 2') :-
   catch(reverse([a|foo], _), error(E, _), true),
   E == type_error(list, foo).

runner:ref(member, 2, calculate_stable, 'XLOG 3.8.3').
runner:case(member, 2, calculate_stable, 'XLOG 3.8.3, XLOG 1') :-
   member(b, [a, b, c]).
runner:case(member, 2, calculate_stable, 'XLOG 3.8.3, XLOG 2') :-
   \+ member(d, [a, b, c]).
runner:case(member, 2, calculate_stable, 'XLOG 3.8.3, XLOG 3') :-
   findall(X, member(X, [a, b, c]), L),
   L == [a, b, c].

runner:ref(select, 3, calculate_stable, 'XLOG 3.8.4').
runner:case(select, 3, calculate_stable, 'XLOG 3.8.4, XLOG 1') :-
   select(b, [a, b, c], X),
   X == [a, c].
runner:case(select, 3, calculate_stable, 'XLOG 3.8.4, XLOG 2') :-
   \+ select(d, [a, b, c], _).
runner:case(select, 3, calculate_stable, 'XLOG 3.8.4, XLOG 3') :-
   findall(X-Y, select(X, [a, b, c], Y), L),
   L == [a-[b, c], b-[a, c], c-[a, b]].
runner:case(select, 3, calculate_stable, 'XLOG 3.8.4, XLOG 4') :-
   findall(X, select(c, X, [a, b]), L),
   L == [[c, a, b], [a, c, b], [a, b, c]].

runner:ref(last, 2, calculate_stable, 'XLOG 3.8.5').
runner:case(last, 2, calculate_stable, 'XLOG 3.8.5, XLOG 1') :-
   last([a, b, c], X),
   X == c.

runner:ref(last, 3, calculate_stable, 'XLOG 3.8.6').
runner:case(last, 3, calculate_stable, 'XLOG 3.8.6, XLOG 1') :-
   last([a, b, c], X, Y),
   X == c, Y == [a, b].
runner:case(last, 3, calculate_stable, 'XLOG 3.8.6, XLOG 2') :-
   last(Y, c, [a, b]),
   Y = [a, b, c].

runner:ref(length, 2, calculate_stable, 'XLOG 3.8.7').
runner:case(length, 2, calculate_stable, 'XLOG 3.8.7, XLOG 1') :-
   length([a, b, c], N),
   N == 3.
runner:case(length, 2, calculate_stable, 'XLOG 3.8.7, XLOG 2') :-
   length(L, 3), length(L, N),
   N == 3.

runner:ref(nth0, 3, calculate_stable, 'XLOG 3.8.8').
runner:case(nth0, 3, calculate_stable, 'XLOG 3.8.8, XLOG 1') :-
   nth0(1, [a, b, c], X),
   X == b.
runner:case(nth0, 3, calculate_stable, 'XLOG 3.8.8, XLOG 2') :-
   nth0(K, [a, b, c], c),
   K == 2.

runner:ref(nth0, 4, calculate_stable, 'XLOG 3.8.9').
runner:case(nth0, 4, calculate_stable, 'XLOG 3.8.9, XLOG 1') :-
   nth0(0, [a, b, c], X, R),
   X == a, R == [b, c].
runner:case(nth0, 4, calculate_stable, 'XLOG 3.8.9, XLOG 2') :-
   nth0(J, [a, b, c], b, R),
   J == 1, R == [a, c].
