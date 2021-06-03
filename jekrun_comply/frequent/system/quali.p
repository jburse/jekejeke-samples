/**
 * Prolog code for the predicate qualification test cases.
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

:- use_module(library(runtime/quali)).

/* var(X) */

runner:ref(var, 1, system_quali, 'XLOG 1.5.1').
/* traditional notation */
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 1 Module') :-
   var(_).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 2 Module') :-
   \+ var(basic/lists:member(_, _)).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 3 Module') :-
   \+ var(foo/bar:baz).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 4 Module') :-
   var(basic/lists:_).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 5 Error') :-
   \+ var(foo/bar:123).
/* array notation */
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 6 Array') :-
   var({_}:new(10, _)).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 7 Array') :-
   \+ var({int}:new(10, _)).
/* object notation */
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 8 Error') :-
   \+ var(37::getx(_)).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 9 Receiver') :-
   var(_/point(3, 7)::getx(_)).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 10 Receiver') :-
   var(geometry/point(3, 7)::_).
runner:case(var, 1, system_quali, 'XLOG 1.5.1, XLOG 11 Receiver') :-
   \+ var(geometry/point(3, 7)::getx(_)).

/* callable(X) */

runner:ref(callable, 1, system_quali, 'XLOG 1.5.2').
/* traditional notation */
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 1 Module') :-
   \+ callable(_).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 2 Module') :-
   callable(basic/lists:member(_, _)).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 3 Module') :-
   callable(foo/bar:baz).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 4 Module') :-
   \+ callable(basic/lists:_).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 5 Error') :-
   \+ callable(foo/bar:123).
/* array notation */
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 6 Array') :-
   \+ callable({_}:new(10, _)).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 7 Array') :-
   callable({int}:new(10, _)).
/* object notation */
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 8 Error') :-
   \+ callable(37::getx(_)).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 9 Receiver') :-
   \+ callable(_/point(3, 7)::getx(_)).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 10 Receiver') :-
   \+ callable(geometry/point(3, 7)::_).
runner:case(callable, 1, system_quali, 'XLOG 1.5.2, XLOG 11 Receiver') :-
   callable(geometry/point(3, 7)::getx(_)).

/* functor(X, F, A) */

runner:ref(functor, 3, system_quali, 'XLOG 1.5.3').
/* traditional notation */
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 1 Error') :-
   catch(functor(_, _, _), error(E, _), true),
   E == instantiation_error.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 2 Module') :-
   functor(M:baz, F, A),
   F == M:baz,
   A == 0.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 3 Error') :-
   catch(functor(foo/bar:_, _, _), error(E, _), true),
   E == instantiation_error.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 4 Module') :-
   functor(basic/lists:member(_, _), F, A),
   F == basic/lists:member,
   A == 2.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 5 Module') :-
   functor(T, basic/lists:member, 2),
   T = basic/lists:member(_, _).
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 6 Module') :-
   functor(foo/bar:baz, F, A),
   F == foo/bar:baz,
   A == 0.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 7 Module') :-
   functor(T, foo/bar:baz, 0),
   T = foo/bar:baz.
/* array notation */
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 8 Array') :-
   functor({int}:new(10, _), F, A),
   F == {int}:new,
   A == 2.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 9 Array') :-
   functor(T, {int}:new, 2),
   T = {int}:new(_, _).
/* receiver notation */
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 10 Receiver') :-
   functor(geometry/point(3, 7)::getx(_), F, A),
   F == geometry/point:getx,
   A == 2.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 11 Receiver') :-
   functor(T, geometry/point:getx, 2),
   T = geometry/point:getx(_, _).
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 12 Receiver') :-
   functor(foo/bar::baz, F, A),
   F == foo/bar:baz,
   A == 1.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 13 Receiver') :-
   functor(T, foo/bar:baz, 1),
   T = foo/bar:baz(_).
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 14 Error') :-
   catch(functor(foo/_::write(abc), _, _), error(E, _), true),
   E = instantiation_error.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 15 Receiver') :-
   functor(M/bar(1, 2)::baz, F, A),
   F == M/bar:baz,
   A == 1.
runner:case(functor, 3, system_quali, 'XLOG 1.5.3, XLOG 16 Receiver') :-
   functor(T, M/bar:baz, 1),
   T = M/bar:baz(_).

/* X =.. Y */

runner:ref(=.., 2, system_quali, 'XLOG 1.5.4').
/* traditional notation */
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 1 Error') :-
   catch(_ =.. _, error(E, _), true),
   E == instantiation_error.
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 2 Module') :-
   M:baz =.. U,
   U == [M:baz].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 3 Error') :-
   catch(foo/bar:_ =.. _, error(E, _), true),
   E == instantiation_error.
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 4 Module') :-
   basic/lists:member(A, B) =.. U,
   U == [basic/lists:member, A, B].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 5 Module') :-
   T =.. [basic/lists:member, A, B],
   T == basic/lists:member(A, B).
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 6 Module') :-
   foo/bar:baz =.. U,
   U == [foo/bar:baz].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 7 Module') :-
   T =.. [foo/bar:baz],
   T == foo/bar:baz.
/* array notation */
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 8 Array') :-
   {int}:new(10, R) =.. U,
   U == [{int}:new, 10, R].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 9 Array') :-
   T =.. [{int}:new, 10, R],
   T == {int}:new(10, R).
/* object notation */
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 10 Receiver') :-
   geometry/point(3, 7)::getx(X) =.. U,
   U == [geometry/point:getx, geometry/point(3, 7), X].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 11 Receiver') :-
   T =.. [geometry/point:getx, geometry/point(3, 7), X],
   T == geometry/point(3, 7)::getx(X).
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 12 Receiver') :-
   foo/bar::baz =.. U,
   U == [foo/bar:baz, foo/bar].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 13 Receiver') :-
   T =.. [foo/bar:baz, foo/bar],
   T == foo/bar::baz.
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 14 Receiver') :-
   T =.. [foo/bar:baz, X],
   T == foo/bar:baz(X).
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 15 Receiver') :-
   T =.. [M:baz, foo/bar],
   T == M:baz(foo/bar).
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 16 Error') :-
   catch(_::write(abc) =.. _, error(E, _), true),
   E == instantiation_error.
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 17 Receiver') :-
   M/bar(1, 2)::baz =.. U,
   U == [M/bar:baz, M/bar(1, 2)].
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 18 Receiver') :-
   T =.. [M/bar:baz, M/bar(1, 2)],
   T == M/bar(1, 2)::baz.
runner:case(=.., 2, system_quali, 'XLOG 1.5.4, XLOG 19 Receiver') :-
   T =.. [M/bar:baz, N/bar(1, 2)],
   T == M/bar:baz(N/bar(1, 2)).

/* sys_extend_term(F, L, T) */

runner:ref(sys_extend_term, 3, system_quali, 'XLOG 1.5.5').
runner:case(sys_extend_term, 3, system_quali, 'XLOG 1.5.5, XLOG 1') :-
   sys_extend_term(functor(F, c), [0], X),
   X == functor(F, c, 0).
runner:case(sys_extend_term, 3, system_quali, 'XLOG 1.5.5, XLOG 2') :-
   sys_extend_term(mod:functor(F, c), [0], X),
   X == mod:functor(F, c, 0).
runner:case(sys_extend_term, 3, system_quali, 'XLOG 1.5.5, XLOG 3') :-
   sys_extend_term(obj(1)::functor(F, c), [0], X),
   X == obj(1)::functor(F, c, 0).

/* sys_shrink_term(T, N, F, L) */

runner:ref(sys_shrink_term, 4, system_quali, 'XLOG 1.5.6').
runner:case(sys_shrink_term, 4, system_quali, 'XLOG 1.5.6, XLOG 1') :-
   sys_shrink_term(functor(F, c, 0), 1, X, Y),
   X == functor(F, c), Y == [0].
runner:case(sys_shrink_term, 4, system_quali, 'XLOG 1.5.6, XLOG 2') :-
   sys_shrink_term(mod:functor(F, c, 0), 1, X, Y),
   X == mod:functor(F, c), Y == [0].
runner:case(sys_shrink_term, 4, system_quali, 'XLOG 1.5.6, XLOG 3') :-
   sys_shrink_term(obj(1)::functor(F, c, 0), 1, X, Y),
   X == obj(1)::functor(F, c), Y == [0].
