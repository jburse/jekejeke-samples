/**
 * Prolog code for the cross test cases.
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

:- use_module(library(term/suspend)).
:- use_module(library(misc/residue)).

/* unify_with_occurs_check(X, Y) */

runner:ref(unify_with_occurs_check, 2, term_cross, 'Term 1.3.1').
runner:case(unify_with_occurs_check, 2, term_cross, 'Term 1.3.1, XLOG 1') :-
   \+ unify_with_occurs_check(A-_, s(A)-n).
runner:case(unify_with_occurs_check, 2, term_cross, 'Term 1.3.1, XLOG 2') :-
   freeze(A, throw(ball)), \+ unify_with_occurs_check(A-_, s(A)-n).
runner:case(unify_with_occurs_check, 2, term_cross, 'Term 1.3.1, XLOG 3') :-
   freeze(A, throw(ball)), freeze(B, true), \+ unify_with_occurs_check(A-B, s(A)-n).

/* subsumes_term(X, Y) */

runner:ref(subsumes_term, 2, term_cross, 'Term 1.3.2').
runner:case(subsumes_term, 2, term_cross, 'Term 1.3.2, XLOG 1') :-
   subsumes_term(_, f(_)).
runner:case(subsumes_term, 2, term_cross, 'Term 1.3.2, XLOG 2') :-
   freeze(Y, throw(ball)), subsumes_term(Y, f(_)).
