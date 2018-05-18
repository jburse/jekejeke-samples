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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

runner:ref(sys_var, 1, system_quali, 'XLOG 3.1').
/* traditional notation */
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 1') :-
   sys_var(_).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 2') :-
   \+ sys_var(basic/lists:member(_,_)).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 3') :-
   \+ sys_var(foo/bar:baz).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 4') :-
   sys_var(basic/lists:_).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 5') :-
   \+ sys_var(foo/bar:123).
/* array notation */
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 6') :-
   sys_var({_}:new(10,_)).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 7') :-
   \+ sys_var({int}:new(10,_)).
/* object notation */
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 8') :-
   \+ sys_var(37::getx(_)).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 9') :-
   sys_var(_/point(3,7)::getx(_)).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 10') :-
   sys_var(geometry/point(3,7)::_).
runner:case(sys_var, 1, system_quali, 'XLOG 3.1, XLOG 11') :-
   \+ sys_var(geometry/point(3,7)::getx(_)).

runner:ref(sys_callable, 1, system_quali, 'XLOG 3.2').
/* traditional notation */
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 1') :-
   \+ sys_callable(_).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 2') :-
   sys_callable(basic/lists:member(_,_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 3') :-
   sys_callable(foo/bar:baz).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 4') :-
   \+ sys_callable(basic/lists:_).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 5') :-
   \+ sys_callable(foo/bar:123).
/* array notation */
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 6') :-
   \+ sys_callable({_}:new(10,_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 7') :-
   sys_callable({int}:new(10,_)).
/* object notation */
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 8') :-
   \+ sys_callable(37::getx(_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 9') :-
   \+ sys_callable(_/point(3,7)::getx(_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 10') :-
   \+ sys_callable(geometry/point(3,7)::_).
runner:case(sys_callable, 1, system_quali, 'XLOG 3.2, XLOG 11') :-
   sys_callable(geometry/point(3,7)::getx(_)).

runner:ref(sys_functor, 3, system_quali, 'XLOG 3.3').
/* traditional notation */
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 1') :-
   catch(sys_functor(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 2') :-
   sys_functor(basic/lists:member(_,_), F, A),
   F == basic/lists:member,
   A == 2.
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 3') :-
   sys_functor(T, basic/lists:member, 2),
   T = basic/lists:member(_,_).
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 4') :-
   sys_functor(foo/bar:baz, F, A),
   F == foo/bar:baz,
   A == 0.
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 5') :-
   sys_functor(T, foo/bar:baz, 0),
   T = foo/bar:baz.
/* array notation */
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 6') :-
   sys_functor({int}:new(10,_), F, A),
   F == {int}:new,
   A == 2.
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 7') :-
   sys_functor(T, {int}:new, 2),
   T = {int}:new(_,_).
/* object notation */
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 8') :-
   sys_functor(geometry/point(3,7)::getx(_), F, A),
   F == geometry/point:getx,
   A == 2.
runner:case(sys_functor, 3, system_quali, 'XLOG 3.3, XLOG 9') :-
   sys_functor(T, geometry/point:getx, 2),
   T = geometry/point:getx(_,_).

runner:ref(sys_univ, 2, system_quali, 'XLOG 3.4').
/* traditional notation */
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 1') :-
   catch(sys_univ(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 2') :-
   sys_univ(basic/lists:member(A,B), U),
   U == [basic/lists:member,A,B].
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 3') :-
   sys_univ(T, [basic/lists:member,A,B]),
   T == basic/lists:member(A,B).
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 4') :-
   sys_univ(foo/bar:baz, U),
   U == [foo/bar:baz].
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 5') :-
   sys_univ(T, [foo/bar:baz]),
   T == foo/bar:baz.
/* array notation */
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 6') :-
   sys_univ({int}:new(10,R), U),
   U == [{int}:new,10,R].
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 7') :-
   sys_univ(T, [{int}:new,10,R]),
   T == {int}:new(10,R).
/* object notation */
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 8') :-
   sys_univ(geometry/point(3,7)::getx(X), U),
   U == [geometry/point:getx,geometry/point(3,7),X].
runner:case(sys_univ, 2, system_quali, 'XLOG 3.4, XLOG 9') :-
   sys_univ(T, [geometry/point:getx,geometry/point(3,7),X]),
   T == geometry/point(3,7)::getx(X).
