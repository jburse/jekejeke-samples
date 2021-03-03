/**
 * Maslov Calculus over Herbrandisized Formulas
 * Prolog flag occurs_check
 * https://stackoverflow.com/q/63798877/502187
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
:- ensure_loaded(aristoteles).
:- ensure_loaded(koutsoukou).
:- ensure_loaded(pelletier).

aristo2 :-
   case(I, _, F),
   form(F, G),
   \+ prove2(G, 3, _),
   write(I),
   write(' failure.'),
   nl,
   fail; true.

pelle2 :-
   pcase(I, _, F),
   \+ prove2(F, 5, _),
   write(I),
   write(' failure.'),
   nl,
   fail; true.

/**
 * prove2(A, M, N):
 * The predicate succeeds whenever a Maslov proof for A can be found.
 * The number of tryed exist/2 rule applications is successively incremented.
 * The parameter M is the maximum number of existential applications,
 * and the result N is the effectively used number.
 */
% prove2(+Form, +Integer, -Integer)
prove2(A, M, N) :-
   norm(A, B),
   herbrand(B, [], 0, _, C),
   between(0, M, N),
   prove2([C], [], N, _).

/**
 * prove2(L, V, N, M):
 * The predicate succeeds whenever a Maslov proof for L can be found.
 * The parameter V is for the current exist/2 environment and the
 * parameter N and M controll the number exist/2 rule applications.
 */
% prove2(+List, +List, +Integer, -Integer)
prove2(L, V, N, M) :- select(or(A, B), L, R), !,
   prove2([A, B|R], V, N, M).
prove2(L, V, N, M) :- select(and(A, B), L, R), !,
   prove2([A|R], V, N, H),
   prove2([B|R], V, H, M).
prove2(L, V, N, M) :- N > 0, H is N-1,
   member(exist(A, B), L),
   copy_term(exist(A, B)-V, exist(C, D)-V),
   prove2([D|L], [C|V], H, M).
prove2(L, _, N, N) :- select(pos(A), L, R),
   member(neg(A), R).
