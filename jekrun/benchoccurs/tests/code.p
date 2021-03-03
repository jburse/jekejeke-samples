/**
 * Test Cases List Types
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information.XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH.If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document.Reproduction
 * of the information is only allowed for non-commercial uses.Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library.Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- ensure_loaded(linter).
:- ensure_loaded(linter2).

case(1, member(X, [X|_])).
case(2, (member(X, [_|L]) :- member(X, L))).

case(3, select(E, [E|L], L)).
case(4, (select(E, [X|L], [X|Y]) :- select(E, L, Y))).

case(5, append([], Z, Z)).
case(6, (append([X|L], Y, [X|Z]) :- append(L, Y, Z))).

case(7, (reverse(X, Y) :- reverse(X, [], Y))).
case(8, reverse([], X, X)).
case(9, (reverse([X|Y], Z, T) :- reverse(Y, [X|Z], T))).

check :-
   case(I, F),
   \+ typeof(F, bool, [], _),
   write(I),
   write(' failure.'),
   nl,
   fail; true.

check2 :-
   case(I, F),
   \+ typeof2(F, bool, [], _),
   write(I),
   write(' failure.'),
   nl,
   fail; true.
