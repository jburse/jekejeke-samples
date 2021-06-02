/**
 * Beckert & Posegga's Algorithm, unify_with_occurs_check/2 call
 *
 * Beckert, B., Posegga, J. leanTAP: Lean tableau-based deduction.
 * J Autom Reasoning 15, 339–358 (1995).
 * https://www.researchgate.net/publication/2250669
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
:- use_module(library(standard/arith)).

:- ensure_loaded(prepare).

/**
 * prove(A, M, N):
 * The predicate succeeds whenever a Maslov proof for A can be found.
 * The number of tryed exist/2 rule applications is successively incremented.
 * The parameter M is the maximum number of existential applications,
 * and the result N is the effectively used number.
 */
% prove(+Form, +Integer, -Integer)
prove(A, M, N) :-
   norm(A, B),
   herbrand(B, [], 0, _, C),
   between(0, M, N),
   prove([C], [], N, _).

/**
 * prove(L, V, N, M):
 * The predicate succeeds whenever a Maslov proof for L can be found.
 * The parameter V is for the current exist/2 environment and the
 * parameter N and M controll the number exist/2 rule applications.
 */
% prove(+List, +List, +Integer, -Integer)
prove(L, V, N, M) :- select(or(A, B), L, R), !,
   prove([A, B|R], V, N, M).
prove(L, V, N, M) :- select(and(A, B), L, R), !,
   prove([A|R], V, N, H),
   prove([B|R], V, H, M).
prove(L, V, N, M) :- N > 0, H is N-1,
   member(exist(A, B), L),
   copy_term(exist(A, B)-V, exist(C, D)-V),
   prove([D|L], [C|V], H, M).
prove(L, _, N, N) :- select(pos(A), L, R),
   member(neg(B), R),
   unify_with_occurs_check(A, B).
