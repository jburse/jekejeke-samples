/**
 * Code Linter, occurs_check flag.
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

% typeof2(+Term, -Term)
typeof2(X, T, L, L) :- var(X),
   member(Y-S, L), Y == X, !,
   S = T.
typeof2(X, T, L, [X-T|L]) :- var(X), !.

typeof2([A|B], list(T), L, R) :-
   typeof2(A, T, L, H),
   typeof2(B, list(T), H, R).
typeof2([], list(_), L, L).

typeof2((A :- B), bool, L, R) :-
   typeof2(A, bool, L, H),
   typeof2(B, bool, H, R).
typeof2((A, B), bool, L, R) :-
   typeof2(A, bool, L, H),
   typeof2(B, bool, H, R).

typeof2(member(A, B), bool, L, R) :-
   typeof2(A, T, L, H),
   typeof2(B, list(T), H, R).
typeof2(select(A, B, C), bool, L, R) :-
   typeof2(A, T, L, H),
   typeof2(B, list(T), H, K),
   typeof2(C, list(T), K, R).
typeof2(append(A, B, C), bool, L, R) :-
   typeof2(A, list(T), L, H),
   typeof2(B, list(T), H, K),
   typeof2(C, list(T), K, R).
typeof2(reverse(A, B), bool, L, R) :-
   typeof2(A, list(T), L, H),
   typeof2(B, list(T), H, R).
typeof2(reverse(A, B, C), bool, L, R) :-
   typeof2(A, list(T), L, H),
   typeof2(B, list(T), H, K),
   typeof2(C, list(T), K, R).
