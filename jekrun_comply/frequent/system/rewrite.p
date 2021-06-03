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
:- dynamic runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(advanced/abstract)).
:- use_module(library(advanced/signal)).
:- use_module(library(basic/lists)).

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

/****************************************************************/
/* Occurs Check                                                 */
/****************************************************************/

/**
 * with_occurs_check(G):
 * The predicate succeeds when ever G succeeds with the occurs check on.
 * The goal should be semi-deterministic, otherwise the occurs check
 * flag change leaks into the continuation.
 */
% with_occurs_check(+Goal)
:- private with_occurs_check/1.
:- meta_predicate with_occurs_check(0).
with_occurs_check(G) :-
   current_prolog_flag(occurs_check, F),
   setup_call_cleanup(
      set_prolog_flag(occurs_check, true),
      G,
      set_prolog_flag(occurs_check, F)).

/* clause head, occurs_check=true */

:- private add/3.
add(n, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

runner:ref(head_flag, 2, system_rewrite, 'XLOG 1.7.1').
runner:case(head_flag, 2, system_rewrite, 'XLOG 1.7.1, XLOG 1') :-
   add(s(n), X, X).
runner:case(head_flag, 2, system_rewrite, 'XLOG 1.7.1, XLOG 2') :-
   \+ with_occurs_check(add(s(n), X, X)).

/* library call, occurs_check=true */

runner:ref(library_flag, 2, system_rewrite, 'XLOG 1.7.2').
runner:case(library_flag, 2, system_rewrite, 'XLOG 1.7.2, XLOG 1') :-
   append([foo], X, X).
runner:case(library_flag, 2, system_rewrite, 'XLOG 1.7.2, XLOG 2') :-
   \+ with_occurs_check(append([foo], X, X)).
