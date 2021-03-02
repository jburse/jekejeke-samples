/**
 * Negation Normal Form and Herbrandization
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

:- op(1200, xfx, -:).

/**
 * herbrand(A, L, N, M, B):
 * The predicate succeeds in B with the herbrand negation
 * inside formula of the negation inside formula A. The
 * herbrandization removes all/2 quantifiers by fresh
 * functions over the exist/2 environment. The parameter L
 * is the exist/2 environment and the parameter N and M are
 * used to generate the fresh functions.
 */
% herbrand(+Norm, +List, +Integer, -Integer, -Herbrand)
herbrand(pos(A), _, N, N, pos(A)).
herbrand(neg(A), _, N, N, neg(A)).
herbrand(or(A, B), L, N, M, or(C, D)) :-
   herbrand(A, L, N, H, C),
   herbrand(B, L, H, M, D).
herbrand(and(A, B), L, N, M, and(C, D)) :-
   herbrand(A, L, N, H, C),
   herbrand(B, L, H, M, D).
herbrand(all(A, B), L, N, M, C) :-
   number_codes(N, R),
   atom_codes(F, [0'$|R]),
   A =.. [F|L],
   H is N+1,
   herbrand(B, L, H, M, C).
herbrand(exist(A, B), L, N, M, exist(A, C)) :-
   herbrand(B, [A|L], N, M, C).

/**
 * norm(A, B):
 * The predicate succeeds in B with the negation inside
 * formula of the formula A. A formula uses connectives
 * (;)/2, (,)/2, (-)/1 and quantifiers all/2, exists/2.
 * Variables or non-connectives and non-quantifiers are
 * considered prime formulas.
 */
% norm(+Form,-Norm)
norm(A, R) :- var(A), !, R = pos(A).
norm((A; B), R) :- !, R = or(C, D), norm(A, C), norm(B, D).
norm((A, B), R) :- !, R = and(C, D), norm(A, C), norm(B, D).
norm((A -: B), R) :- !, norm((-A; B), R).
norm(all(A, B), R) :- !, R = all(A, C), norm(B, C).
norm(exist(A, B), R) :- !, R = exist(A, C), norm(B, C).
norm(-A, R) :- var(A), !, R = neg(A).
norm(- (A; B), R) :- !, R = and(C, D), norm(-A, C), norm(-B, D).
norm(- (A, B), R) :- !, R = or(C, D), norm(-A, C), norm(-B, D).
norm(- (A -: B), R) :- !, norm((A, -B), R).
norm(-all(A, B), R) :- !, R = exist(A, C), norm(-B, C).
norm(-exist(A, B), R) :- !, R = all(A, C), norm(-B, C).
norm(- -A, R) :- !, norm(A, R).
norm(-A, R) :- !, R = neg(A).
norm(A, pos(A)).
