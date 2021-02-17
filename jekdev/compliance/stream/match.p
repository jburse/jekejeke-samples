/**
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

/****************************************************************/
/* Stream Control                                               */
/****************************************************************/

/* subsumes_term(X, Y) */

runner:ref(subsumes, 2, stream_match, 'XLOG 4.1.1').
runner:case(subsumes, 2, stream_match, 'XLOG 4.1.1, XLOG 1') :-
   subsumes(a, a).
runner:case(subsumes, 2, stream_match, 'XLOG 4.1.1, XLOG 2') :-
   subsumes(f(A, B), f(Z, Z)), A == Z, B == Z.
runner:case(subsumes, 2, stream_match, 'XLOG 4.1.1, XLOG 3') :-
   \+ subsumes(f(Z, Z), f(_, _)).
runner:case(subsumes, 2, stream_match, 'XLOG 4.1.1, XLOG 4') :-
   \+ subsumes(g(X), g(f(X))).
runner:case(subsumes, 2, stream_match, 'XLOG 4.1.1, XLOG 6') :-
   subsumes(X, Y), \+ subsumes(Y, f(X)).

/* ?-(X, Y) */

:- dynamic pattern/2.
pattern(f(_), foo).
pattern(f(_), X) ?- X = bar.
pattern(f(_), baz).

runner:ref(?-, 2, stream_match, 'XLOG 4.1.2').
runner:case(?-, 2, stream_match, 'XLOG 4.1.2, XLOG 1') :-
   findall(X, pattern(_, X), L),
   L == [foo, baz].
runner:case(?-, 2, stream_match, 'XLOG 4.1.2, XLOG 2') :-
   findall(X, clause(pattern(_, X), _), L),
   L = [foo, _, baz].
