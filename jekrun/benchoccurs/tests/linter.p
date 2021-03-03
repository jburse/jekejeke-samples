/**
 * Code Linter, unify_with_occurs_check/2 call.
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

:- use_module(library(basic/lists)).

% typeof(+Term, -Term)
typeof(X, T, L, L) :- var(X),
   member(Y-S, L), Y == X, !,
   unify_with_occurs_check(S, T).
typeof(X, T, L, [X-T|L]) :- var(X), !.

typeof([A|B], list(T), L, R) :-
   typeof(A, T, L, H),
   typeof(B, list(T), H, R).
typeof([], list(_), L, L).

typeof((A :- B), bool, L, R) :-
   typeof(A, bool, L, H),
   typeof(B, bool, H, R).
typeof((A, B), bool, L, R) :-
   typeof(A, bool, L, H),
   typeof(B, bool, H, R).

typeof(member(A, B), bool, L, R) :-
   typeof(A, T, L, H),
   typeof(B, list(T), H, R).
typeof(select(A, B, C), bool, L, R) :-
   typeof(A, T, L, H),
   typeof(B, list(T), H, K),
   typeof(C, list(T), K, R).
typeof(append(A, B, C), bool, L, R) :-
   typeof(A, list(T), L, H),
   typeof(B, list(T), H, K),
   typeof(C, list(T), K, R).
typeof(reverse(A, B), bool, L, R) :-
   typeof(A, list(T), L, H),
   typeof(B, list(T), H, R).
typeof(reverse(A, B, C), bool, L, R) :-
   typeof(A, list(T), L, H),
   typeof(B, list(T), H, K),
   typeof(C, list(T), K, R).


