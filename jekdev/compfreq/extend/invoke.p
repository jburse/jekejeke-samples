/**
 * Prolog code for the t.b.d. test cases.
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

:- use_module(library(advanced/arith)).
:- use_module(library(advanced/sequence)).

/* limit(N, G) */

runner:ref(limit, 2, extend_invoke, 'XLOG 4.1').
runner:case(limit, 2, extend_invoke, 'XLOG 4.1, XLOG 1') :-
   findall(X, limit(3, between(5, 10, X)), L),
   L == [5, 6, 7].
runner:case(limit, 2, extend_invoke, 'XLOG 4.1, XLOG 2') :-
   findall(X, limit(3, between(5, 6, X)), L),
   L == [5, 6].
runner:case(limit, 2, extend_invoke, 'XLOG 4.1, XLOG 3') :-
   \+ limit(0, repeat).
runner:case(limit, 2, extend_invoke, 'XLOG 4.1, XLOG 4') :-
   catch(limit(_, _), error(E, _), true),
   E == instantiation_error.

/* offset(N, G) */

runner:ref(offset, 2, extend_invoke, 'XLOG 4.2').
runner:case(offset, 2, extend_invoke, 'XLOG 4.2, XLOG 1') :-
   findall(X, offset(3, between(5, 10, X)), L),
   L == [8, 9, 10].
runner:case(offset, 2, extend_invoke, 'XLOG 4.2, XLOG 2') :-
   \+ offset(3, between(5, 6, _)).
runner:case(offset, 2, extend_invoke, 'XLOG 4.2, XLOG 3') :-
   findall(X, limit(5, offset(3, between(1, 10, X))), L),
   L == [4, 5, 6, 7, 8].
runner:case(offset, 2, extend_invoke, 'XLOG 4.2, XLOG 4') :-
   catch(offset(_, _), error(E, _), true),
   E == instantiation_error.

/* call_nth(G, N) */

runner:ref(call_nth, 2, extend_invoke, 'XLOG 4.3').
runner:case(call_nth, 2, extend_invoke, 'XLOG 4.3, XLOG 1') :-
   findall(N, call_nth(between(5, 10, _), N), L),
   L == [1, 2, 3, 4, 5, 6].
runner:case(call_nth, 2, extend_invoke, 'XLOG 4.3, XLOG 2') :-
   call_nth(between(5, 10, X), 3),
   X == 7.
runner:case(call_nth, 2, extend_invoke, 'XLOG 4.3, XLOG 3') :-
   \+ call_nth(repeat, 0).
runner:case(call_nth, 2, extend_invoke, 'XLOG 4.3, XLOG 4') :-
   catch(call_nth(_, _), error(E, _), true),
   E == instantiation_error.

/* foreach(G, T) */

alpha(1).
alpha(2).
alpha(3).
beta(1, a).
beta(2, b).
beta(3, c).

runner:ref(foreach, 2, extend_invoke, 'XLOG 4.4').
runner:case(foreach, 2, extend_invoke, 'XLOG 4.4, XLOG 1') :-
   foreach(fail, true).
runner:case(foreach, 2, extend_invoke, 'XLOG 4.4, XLOG 2') :-
   foreach(alpha(X), Y^beta(X, Y)).
runner:case(foreach, 2, extend_invoke, 'XLOG 4.4, XLOG 3') :-
   \+ foreach(alpha(X), Y^beta(Y, X)).
runner:case(foreach, 2, extend_invoke, 'XLOG 4.4, XLOG 4') :-
   Y = foo(_, _, _), foreach(between(1, 3, X), arg(X, Y, X)),
   Y == foo(1, 2, 3).
runner:case(foreach, 2, extend_invoke, 'XLOG 4.4, XLOG 5') :-
   catch(foreach(_, beta(_, _)), error(E, _), true),
   E == instantiation_error.
runner:case(foreach, 2, extend_invoke, 'XLOG 4.4, XLOG 6') :-
   catch(foreach(alpha(_), 1), error(E, _), true),
   E == type_error(callable, 1).

/* foreach(G, T, I, O) */

'C'(X, [X|O], O).

runner:ref(foreach, 4, extend_invoke, 'XLOG 4.5').
runner:case(foreach, 4, extend_invoke, 'XLOG 4.5, XLOG 1') :-
   foreach(fail, 'C'(1), I, O),
   O == I.
runner:case(foreach, 4, extend_invoke, 'XLOG 4.5, XLOG 2') :-
   foreach(between(1, 3, X), 'C'(X), I, O),
   I == [1, 2, 3|O].
runner:case(foreach, 4, extend_invoke, 'XLOG 4.5, XLOG 3') :-
   foreach(between(1, 3, _), 'C'(Y), I, O),
   I == [Y, Y, Y|O].
runner:case(foreach, 4, extend_invoke, 'XLOG 4.5, XLOG 4') :-
   catch(foreach(_, 'C'(1), _, _), error(E, _), true),
   E == instantiation_error.
runner:case(foreach, 4, extend_invoke, 'XLOG 4.5, XLOG 5') :-
   catch(foreach(alpha(_), 1, _, _), error(E, _), true),
   E == type_error(callable, 1).
