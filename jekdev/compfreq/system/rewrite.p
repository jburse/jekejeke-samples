/**
 * Prolog code for the term expansion test cases.
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

/* expand_goal(S, T) */

runner:ref(expand_goal, 2, system_rewrite, 'XLOG 1.6.1').
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 1') :-
   expand_goal(((a, b), c), X),
   X == (a, b, c).
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 2') :-
   expand_goal((a, true), X),
   X == a.
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 3') :-
   expand_goal((true, a), X),
   X == a.
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 4') :-
   expand_goal((a, true, b), X),
   X == (a, b).
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 5') :-
   expand_goal((A, true), X),
   A == X.

/* expand_term(S, T) */

runner:ref(expand_term, 2, system_rewrite, 'XLOG 1.6.2').
runner:case(expand_term, 2, system_rewrite, 'XLOG 1.6.2, XLOG 1') :-
   expand_term(((a :- b) :- c), X),
   X == (a :- c, b).
runner:case(expand_term, 2, system_rewrite, 'XLOG 1.6.2, XLOG 2') :-
   expand_term((a :- true), X),
   X == a.
runner:case(expand_term, 2, system_rewrite, 'XLOG 1.6.2, XLOG 3') :-
   expand_term((A :- true), X),
   A == X.
