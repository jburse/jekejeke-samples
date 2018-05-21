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

runner:ref(sys_var, 1, system_quali, 'XLOG 4.1').
/* traditional notation */
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 1 Module') :-
   sys_var(_).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 2 Module') :-
   \+ sys_var(basic/lists:member(_,_)).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 3 Module') :-
   \+ sys_var(foo/bar:baz).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 4 Module') :-
   sys_var(basic/lists:_).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 5 Error') :-
   \+ sys_var(foo/bar:123).
/* array notation */
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 6 Array') :-
   sys_var({_}:new(10,_)).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 7 Array') :-
   \+ sys_var({int}:new(10,_)).
/* object notation */
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 8 Error') :-
   \+ sys_var(37::getx(_)).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 9 Receiver') :-
   sys_var(_/point(3,7)::getx(_)).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 10 Receiver') :-
   sys_var(geometry/point(3,7)::_).
runner:case(sys_var, 1, system_quali, 'XLOG 4.1, XLOG 11 Receiver') :-
   \+ sys_var(geometry/point(3,7)::getx(_)).

runner:ref(sys_callable, 1, system_quali, 'XLOG 4.2').
/* traditional notation */
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 1 Module') :-
   \+ sys_callable(_).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 2 Module') :-
   sys_callable(basic/lists:member(_,_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 3 Module') :-
   sys_callable(foo/bar:baz).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 4 Module') :-
   \+ sys_callable(basic/lists:_).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 5 Error') :-
   \+ sys_callable(foo/bar:123).
/* array notation */
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 6 Array') :-
   \+ sys_callable({_}:new(10,_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 7 Array') :-
   sys_callable({int}:new(10,_)).
/* object notation */
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 8 Error') :-
   \+ sys_callable(37::getx(_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 9 Receiver') :-
   \+ sys_callable(_/point(3,7)::getx(_)).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 10 Receiver') :-
   \+ sys_callable(geometry/point(3,7)::_).
runner:case(sys_callable, 1, system_quali, 'XLOG 4.2, XLOG 11 Receiver') :-
   sys_callable(geometry/point(3,7)::getx(_)).

runner:ref(sys_functor, 3, system_quali, 'XLOG 4.3').
/* traditional notation */
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 1 Error') :-
   catch(sys_functor(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 2 Module') :-
   sys_functor(M:baz, F, A),
   F == M:baz,
   A == 0.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 3 Error') :-
   catch(sys_functor(foo/bar:_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 4 Module') :-
   sys_functor(basic/lists:member(_,_), F, A),
   F == basic/lists:member,
   A == 2.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 5 Module') :-
   sys_functor(T, basic/lists:member, 2),
   T = basic/lists:member(_,_).
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 6 Module') :-
   sys_functor(foo/bar:baz, F, A),
   F == foo/bar:baz,
   A == 0.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 7 Module') :-
   sys_functor(T, foo/bar:baz, 0),
   T = foo/bar:baz.
/* array notation */
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 8 Array') :-
   sys_functor({int}:new(10,_), F, A),
   F == {int}:new,
   A == 2.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 9 Array') :-
   sys_functor(T, {int}:new, 2),
   T = {int}:new(_,_).
/* receiver notation */
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 10 Receiver') :-
   sys_functor(geometry/point(3,7)::getx(_), F, A),
   F == geometry/point:getx,
   A == 2.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 11 Receiver') :-
   sys_functor(T, geometry/point:getx, 2),
   T = geometry/point:getx(_,_).
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 12 Receiver') :-
   sys_functor(foo/bar::baz, F, A),
   F == foo/bar:baz,
   A == 1.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 13 Receiver') :-
   sys_functor(T, foo/bar:baz, 1),
   T = foo/bar:baz(_).
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 14 Error') :-
   catch(sys_functor(foo/_::write(abc), _, _), error(E,_), true),
   E = domain_error(receiver,foo/_).
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 15 Receiver') :-
   sys_functor(M/bar(1,2)::baz, F, A),
   F == M/bar:baz,
   A == 1.
runner:case(sys_functor, 3, system_quali, 'XLOG 4.3, XLOG 16 Receiver') :-
   sys_functor(T, M/bar:baz, 1),
   T = M/bar:baz(_).

runner:ref(sys_univ, 2, system_quali, 'XLOG 4.4').
/* traditional notation */
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 1 Error') :-
   catch(sys_univ(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 2 Module') :-
   sys_univ(M:baz, U),
   U == [M:baz].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 3 Error') :-
   catch(sys_univ(foo/bar:_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 4 Module') :-
   sys_univ(basic/lists:member(A,B), U),
   U == [basic/lists:member,A,B].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 5 Module') :-
   sys_univ(T, [basic/lists:member,A,B]),
   T == basic/lists:member(A,B).
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 6 Module') :-
   sys_univ(foo/bar:baz, U),
   U == [foo/bar:baz].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 7 Module') :-
   sys_univ(T, [foo/bar:baz]),
   T == foo/bar:baz.
/* array notation */
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 8 Array') :-
   sys_univ({int}:new(10,R), U),
   U == [{int}:new,10,R].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 9 Array') :-
   sys_univ(T, [{int}:new,10,R]),
   T == {int}:new(10,R).
/* object notation */
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 10 Receiver') :-
   sys_univ(geometry/point(3,7)::getx(X), U),
   U == [geometry/point:getx,geometry/point(3,7),X].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 11 Receiver') :-
   sys_univ(T, [geometry/point:getx,geometry/point(3,7),X]),
   T == geometry/point(3,7)::getx(X).
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 12 Receiver') :-
   sys_univ(foo/bar::baz, U),
   U == [foo/bar:baz,foo/bar].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 13 Receiver') :-
   sys_univ(T, [foo/bar:baz,foo/bar]),
   T == foo/bar::baz.
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 14 Receiver') :-
   sys_univ(T, [foo/bar:baz,X]),
   T == foo/bar:baz(X).
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 15 Receiver') :-
   sys_univ(T, [M:baz,foo/bar]),
   T == M:baz(foo/bar).
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 16 Error') :-
   catch(sys_univ(_::write(abc), _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 17 Receiver') :-
   sys_univ(M/bar(1,2)::baz, U),
   U == [M/bar:baz,M/bar(1,2)].
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 18 Receiver') :-
   sys_univ(T, [M/bar:baz,M/bar(1,2)]),
   T == M/bar(1,2)::baz.
runner:case(sys_univ, 2, system_quali, 'XLOG 4.4, XLOG 19 Receiver') :-
   sys_univ(T, [M/bar:baz,N/bar(1,2)]),
   T == M/bar:baz(N/bar(1,2)).

runner:ref(sys_get_module, 2, system_quali, 'XLOG 3.5').
runner:case(sys_get_module, 2, system_quali, 'XLOG 3.5, XLOG 1 Error') :-
   catch(sys_get_module(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_get_module, 2, system_quali, 'XLOG 3.5, XLOG 2 Compound') :-
   sys_get_module(beta(3,7), X),
   X == beta.
