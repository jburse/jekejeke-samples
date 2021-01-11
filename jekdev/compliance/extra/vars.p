/**
 * Prolog code for the extra vars test cases.
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

:- use_module(library(standard/arith)).
:- use_module(library(system/charsio)).
:- use_module(library(advanced/signal)).

/****************************************************************/
/* vars.p extras                                               */
/****************************************************************/

/* numbervars(T, N, M) */

runner:ref(numbervars, 3, extra_vars, 'XLOG 3.1.1').
runner:case(numbervars, 3, extra_vars, 'XLOG 3.1.1, XLOG 1') :-
   numbervars(f(X, X, Y), 0, N),
   X == '$VAR'(0), Y == '$VAR'(1), N == 2.
runner:case(numbervars, 3, extra_vars, 'XLOG 3.1.1, XLOG 2') :-
   numbervars(f(Y, X, X), 10, N),
   Y == '$VAR'(10), X == '$VAR'(11), N == 12.
runner:case(numbervars, 3, extra_vars, 'XLOG 3.1.1, XLOG 3') :-
   catch(numbervars(_, a, _), error(E, _), true),
   nonvar(E), E = type_error(integer, _).
runner:case(numbervars, 3, extra_vars, 'XLOG 3.1.1, XLOG 4') :-
   catch(numbervars(_, -2, _), error(E, _), true),
   E == representation_error(not_less_than_zero).

/* nonground(X, Y) */

runner:ref(nonground, 2, extra_vars, 'XLOG 3.1.2').
runner:case(nonground, 2, extra_vars, 'XLOG 3.1.2, XLOG 1') :-
   \+ nonground(foo, _).
runner:case(nonground, 2, extra_vars, 'XLOG 3.1.2, XLOG 2') :-
   nonground(bar(X, _, X), Z), Z == X.

/****************************************************************/
/* logic.p extras                                               */
/****************************************************************/

/* A *-> B */

runner:ref(*->, 2, extra_vars, 'XLOG 3.2.1').
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 1') :-
   true *-> true.
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 2') :-
   \+ (true *-> fail).
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 3') :-
   \+ (fail *-> true).
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 4a') :-
   (true *-> X = 1), X == 1.
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 4b') :-
   findall(X, (true *-> X = 1), [_]).
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 5a') :-
   ((X = 1; X = 2) *-> true), X == 1.
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 5b') :-
   findall(X, ((X = 1; X = 2) *-> true), [_, X|_]), X == 2.
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 5c') :-
   findall(X, ((X = 1; X = 2) *-> true), [_, _]).
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 6a') :-
   (true *-> (X = 1; X = 2)), X == 1.
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 6b') :-
   findall(X, (true *-> (X = 1; X = 2)), [_, X|_]), X == 2.
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, ISO 6c') :-
   findall(X, (true *-> (X = 1; X = 2)), [_, _]).
runner:case(*->, 2, extra_vars, 'XLOG 3.2.1, XLOG 1') :-
   findall(X-Y, ((Y = 1; Y = 2), ((X = 1, !; X = 2) *-> true)), [_, _]).

/* A *-> B ; C */

runner:ref(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2').
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 1') :-
   true *-> true; fail.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 2') :-
   fail *-> true; true.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 3') :-
   \+ (true *-> fail; fail).
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 4') :-
   \+ (fail *-> true; fail).
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 5') :-
   (true *-> X = 1; X = 2), X == 1.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 6') :-
   (fail *-> X = 1; X = 2), X == 2.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 7a') :-
   (true *-> (X = 1; X = 2); true), X == 1.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 7b') :-
   findall(X, (true *-> (X = 1; X = 2); true), [_, X|_]), X == 2.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 7c') :-
   findall(X, (true *-> (X = 1; X = 2); true), [_, _]).
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 8a') :-
   ((X = 1; X = 2) *-> true; true), X == 1.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 8b') :-
   findall(X, ((X = 1; X = 2) *-> true; true), [_, X|_]), X == 2.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 8C') :-
   findall(X, ((X = 1; X = 2) *-> true; true), [_, _]).
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, ISO 9') :-
   (! *-> fail), true; true.
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, XLOG 1') :-
   findall(X-Y, ((Y = 1; Y = 2), ((X = 1; X = 2) *-> true; true)), [_, _, _, _]).
runner:case(soft_if_then_else, 3, extra_vars, 'XLOG 3.2.2, XLOG 2') :-
   findall(X-Y, ((Y = 1; Y = 2), ((X = 1, !; X = 2) *-> true; true)), [_, _]).

/* forall(G, T) */

alpha(1).
alpha(2).
alpha(3).
beta(1, a).
beta(2, b).
beta(3, c).

runner:ref(forall, 2, extra_vars, 'N208 8.10.4').
runner:case(forall, 2, extra_vars, 'N208 8.10.4, XLOG 1') :-
   forall(fail, true).
runner:case(forall, 2, extra_vars, 'N208 8.10.4, XLOG 2') :-
   forall(alpha(X), beta(X, _)).
runner:case(forall, 2, extra_vars, 'N208 8.10.4, XLOG 3') :-
   \+ forall(alpha(X), beta(_, X)).
runner:case(forall, 2, extra_vars, 'N208 8.10.4, XLOG 4') :-
   Y = foo(A, B, C), forall(between(1, 3, X), arg(X, Y, X)),
   Y == foo(A, B, C).
runner:case(forall, 2, extra_vars, 'N208 8.10.4, XLOG 5') :-
   catch(forall(_, beta(_, _)), error(E, _), true),
   E == instantiation_error.
runner:case(forall, 2, extra_vars, 'N208 8.10.4, XLOG 6') :-
   catch(forall(alpha(_), 1), error(E, _), true),
   E == type_error(callable, 1).

/* findall(T, G, L, R) */

runner:ref(findall, 4, extra_vars, 'XLOG 3.2.3').
runner:case(findall, 4, extra_vars, 'XLOG 3.2.3, XLOG 1') :-
   findall(X, (X = 1; X = 2), S, T),
   S = [1, 2|T].
runner:case(findall, 3, extra_vars, 'XLOG 3.2.3, XLOG 2') :-
   \+ findall(X, (X = 1; X = 2), [2, 1|T], T).

/****************************************************************/
/* call.p extras                                                */
/****************************************************************/

/* callable_property(C, P) */

runner:ref(callable_property, 2, extra_vars, 'XLOG 3.3.1').
runner:case(callable_property, 2, extra_vars, 'XLOG 3.3.1, XLOG 1') :-
   callable_property(here, sys_context(C)),
   \+ C = ''.
runner:case(callable_property, 2, extra_vars, 'XLOG 3.3.1, XLOG 2') :-
   set_callable_property(H, sys_context(''), here),
   H = here.
runner:case(callable_property, 2, extra_vars, 'XLOG 3.3.1, XLOG 3') :-
   set_callable_property(H, sys_context(''), here),
   callable_property(H, sys_context(C)),
   C = ''.
runner:case(callable_property, 2, extra_vars, 'XLOG 3.3.1, XLOG 4') :-
   catch(callable_property(1, sys_context(_)), error(E, _), true),
   E == type_error(callable, 1).

/****************************************************************/
/* Syntax extras                                                */
/****************************************************************/

/* dot syntax */

:- op(200, xfy, '.').
:- set_oper_property(infix('.'), sys_alias(sys_dot)).
:- op(200, xfy, sys_dot).
:- set_oper_property(infix(sys_dot), sys_portray('.')).

runner:ref(dot_syntax, 2, extra_vars, 'XLOG 3.4.1').
runner:case(dot_syntax, 2, extra_vars, 'XLOG 3.4.1, XLOG 1') :-
   with_output_to(atom(A), write_canonical(foo.bar)),
   A == 'sys_dot(foo, bar)'.
runner:case(dot_syntax, 2, extra_vars, 'XLOG 3.4.1, XLOG 2') :-
   with_output_to(atom(A), write(foo.bar)),
   A == 'foo.bar'.
runner:case(dot_syntax, 2, extra_vars, 'XLOG 3.4.1, XLOG 3') :-
   with_output_to(atom(A), write_canonical([foo|bar])),
   A == '''.''(foo, bar)'.
runner:case(dot_syntax, 2, extra_vars, 'XLOG 3.4.1, XLOG 4') :-
   with_output_to(atom(A), write([foo|bar])),
   A == '[foo|bar]'.

/* set syntax */

:- op(300, fx, {}).
:- set_oper_property(prefix({}), sys_alias(sys_set)).
:- op(300, fx, sys_set).
:- set_oper_property(prefix(sys_set), sys_portray({})).

runner:ref(set_syntax, 2, extra_vars, 'XLOG 3.4.2').
runner:case(set_syntax, 2, extra_vars, 'XLOG 3.4.2, XLOG 1') :-
   with_output_to(atom(A), write_canonical({}foo)),
   A == 'sys_set(foo)'.
runner:case(set_syntax, 2, extra_vars, 'XLOG 3.4.2, XLOG 2') :-
   with_output_to(atom(A), write({}foo)),
   A == '{}foo'.
runner:case(set_syntax, 2, extra_vars, 'XLOG 3.4.2, XLOG 3') :-
   with_output_to(atom(A), write_canonical({foo})),
   A == '{}(foo)'.
runner:case(set_syntax, 2, extra_vars, 'XLOG 3.4.2, XLOG 4') :-
   with_output_to(atom(A), write({foo})),
   A == '{foo}'.

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

/* X = Y, occurs_check=true */

runner:ref(unify_flag, 2, extra_vars, 'XLOG 3.5.1').
runner:case(unify_flag, 2, extra_vars, 'XLOG 3.5.1, XLOG 1') :-
   X = f(X).
runner:case(unify_flag, 2, extra_vars, 'XLOG 3.5.1, XLOG 2') :-
   \+ with_occurs_check(X = f(X)).

/* X \= Y, occurs_check=true */

runner:ref(not_unify_flag, 2, extra_vars, 'XLOG 3.5.2').
runner:case(not_unify_flag, 2, extra_vars, 'XLOG 3.5.2, XLOG 1') :-
   \+ X \= f(X).
runner:case(not_unify_flag, 2, extra_vars, 'XLOG 3.5.2, XLOG 2') :-
   with_occurs_check(X \= f(X)).
