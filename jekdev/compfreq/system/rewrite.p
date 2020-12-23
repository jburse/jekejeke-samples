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

:- use_module(library(advanced/abstract)).

:- multifile goal_expansion/2.
:- meta_predicate goal_expansion(0, 0).
goal_expansion(println(X), (write(X), nl)).
goal_expansion(double(X, Y), Y is 2*X).

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
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 6') :-
   expand_goal(println('Hello World!'), X),
   X == (write('Hello World!'), nl).
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 7') :-
   expand_goal(call(println, 'Hello World!'), X),
   X = call(A\(write(A), nl), 'Hello World!').
runner:case(expand_goal, 2, system_rewrite, 'XLOG 1.6.1, XLOG 8') :-
   expand_goal(7 is 1+double(3), X),
   X = (7 is 1+A\(A is 2*3)).

/* expand_term(S, T) */

runner:ref(expand_term, 2, system_rewrite, 'XLOG 1.6.2').
runner:case(expand_term, 2, system_rewrite, 'XLOG 1.6.2, XLOG 1') :-
   true.

/* term_singletons(S, T) */

runner:ref(term_singletons, 2, system_rewrite, 'XLOG 1.6.3').
runner:case(term_singletons, 2, system_rewrite, 'XLOG 1.6.3, XLOG 1') :-
   term_singletons(t, L), L == [].
runner:case(term_singletons, 2, system_rewrite, 'XLOG 1.6.3, XLOG 2') :-
   term_singletons(a([], X), L), L == [X].
runner:case(term_singletons, 2, system_rewrite, 'XLOG 1.6.3, XLOG 3') :-
   C = 3.3*A, term_singletons(A+B/C, L), L == [B].

/* sys_goal_kernel(E, K) */

runner:ref(sys_goal_kernel, 2, system_rewrite, 'XLOG 1.6.4').
runner:case(sys_goal_kernel, 2, system_rewrite, 'XLOG 1.6.4, XLOG 1') :-
   sys_goal_kernel(p(X, Y), K),
   K == p(X, Y).
runner:case(sys_goal_kernel, 2, system_rewrite, 'XLOG 1.6.4, XLOG 2') :-
   sys_goal_kernel(Z^(p(X, Z), q(Z, Y)), K),
   K == (p(X, Z), q(Z, Y)).

/* sys_goal_globals(E, G) */

runner:ref(sys_goal_globals, 2, system_rewrite, 'XLOG 1.6.5').
runner:case(sys_goal_globals, 2, system_rewrite, 'XLOG 1.6.5, XLOG 1') :-
   sys_goal_globals(p(X, Y), G),
   G == [X, Y].
runner:case(sys_goal_globals, 2, system_rewrite, 'XLOG 1.6.5, XLOG 2') :-
   sys_goal_globals(Z^(p(X, Z), q(Z, Y)), G),
   G == [X, Y].

/* sys_extend_args(F, A, G) */
/* sys_extend_args(F, A, B, G) */
/* etc.. */

runner:ref(sys_extend_args, 3, system_rewrite, 'XLOG 1.6.6').
runner:case(sys_extend_args, 3, system_rewrite, 'XLOG 1.6.6, XLOG 1') :-
   sys_extend_args(integer, 3, X),
   X == integer(3).
runner:case(sys_extend_args, 3, system_rewrite, 'XLOG 1.6.6, XLOG 2') :-
   sys_extend_args(functor(F, c), 0, X),
   X == functor(F, c, 0).
runner:case(sys_extend_args, 3, system_rewrite, 'XLOG 1.6.6, XLOG 3') :-
   sys_extend_args(;, A = 1, B = 2, R),
   R == (A = 1; B = 2).

/* sys_shrink_args(F, A, G) */
/* sys_shrink_args(F, A, B, G) */
/* etc.. */

runner:ref(sys_shrink_args, 3, system_rewrite, 'XLOG 1.6.7').
runner:case(sys_shrink_args, 3, system_rewrite, 'XLOG 1.6.7, XLOG 1') :-
   sys_shrink_args(X, Y, integer(3)),
   X == integer, Y == 3.
runner:case(sys_shrink_args, 3, system_rewrite, 'XLOG 1.6.7, XLOG 2') :-
   sys_shrink_args(X, Y, functor(F, c, 0)),
   X == functor(F, c), Y == 0.
runner:case(sys_shrink_args, 3, system_rewrite, 'XLOG 1.6.7, XLOG 3') :-
   sys_shrink_args(X, Y, Z, (A = 1; B = 2)),
   X == ;, Y == (A = 1), Z == (B = 2).
