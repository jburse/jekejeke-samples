/**
 * Prolog code for the type inference with explicit occurs check.
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

:- use_module(library(basic/lists)).

% typed(+Expr, +List, -Type)
typed(X, C, A) :- var(X), !,
   member(Y-B, C), Y == X, !,
   unify_with_occurs_check(B, A).
typed(lam(X, T), C, (A -> B)) :-
   typed(T, [X-A|C], B).
typed(app(S, T), C, B) :-
   typed(S, C, (A -> B)),
   typed(T, C, A).

% ?- typed(app(X,X), [X-A], B).
% No

% ?- typed(app(X,Y), [X-A,Y-B], C).
% A = (B -> C)

% ?- typed(lam(X,lam(Y,app(Y,X))), [], A).
% A = (_A -> (_A -> _B) -> _B)

