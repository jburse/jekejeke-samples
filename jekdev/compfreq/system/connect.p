/**
 * Prolog code for the connectivity test cases.
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

:- use_module(library(system/uri)).
:- use_module(library(system/domain)).

/* make_query(N, V, R, Q) */

runner:ref(make_query, 4, system_connect, 'XLOG 1.1.1').
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 1') :-
   make_query(foo, bar, '', Q),
   Q == 'foo=bar'.
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 2') :-
   make_query(V, N, R, 'foo=bar'),
   V == foo, N == bar, R == ''.
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 3') :-
   make_query(foo, bar, 'foo=baz&jack=jill', Q),
   Q == 'foo=bar&foo=baz&jack=jill'.
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 4') :-
   make_query(V, N, R, 'foo=bar&foo=baz&jack=jill'),
   V == foo, N == bar, R == 'foo=baz&jack=jill'.
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 5') :-
   catch(make_query(_, _, _, _), error(E, _), true),
   E == instantiation_error.
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 6') :-
   make_query(foo, baür, 'foo=ba%C3%B6z', Q),
   Q = 'foo=baür&foo=ba%C3%B6z'.
runner:case(make_query, 4, system_connect, 'XLOG 1.1.1, XLOG 7') :-
   make_query(N, V, R, 'foo=ba%C3%BCr&foo=ba%C3%B6z'),
   N == foo, V == baür, R == 'foo=ba%C3%B6z'.

/* make_uri(S, Q, H, U) */

runner:ref(make_uri, 4, system_connect, 'XLOG 1.1.2').
runner:case(make_uri, 4, system_connect, 'XLOG 1.1.2, XLOG 1') :-
   make_uri('/example.org/foo', bar, baz, U),
   U == '/example.org/foo?bar#baz'.
runner:case(make_uri, 4, system_connect, 'XLOG 1.1.2, XLOG 2') :-
   make_uri(S, Q, H, '/example.org/foo?bar#baz'),
   S == '/example.org/foo', Q == bar, H == baz.
runner:case(make_uri, 4, system_connect, 'XLOG 1.1.2, XLOG 3') :-
   catch(make_uri(_, _, foo, _), error(E, _), true),
   E == instantiation_error.
runner:case(make_uri, 4, system_connect, 'XLOG 1.1.2, XLOG 4') :-
   make_uri('/example.org/foäo', 'ba%C3%B6r', baüz, U),
   U == '/example.org/foäo?ba%C3%B6r#baüz'.
runner:case(make_uri, 4, system_connect, 'XLOG 1.1.2, XLOG 5') :-
   make_uri(S, Q, H, '/example.org/fo%C3%A4o?ba%C3%B6r#ba%C3%BCz'),
   S == '/example.org/foäo', Q == 'ba%C3%B6r', H == baüz.

/* uri_encode(U, E) */

runner:ref(uri_encode, 2, system_connect, 'XLOG 1.1.3').
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 1') :-
   uri_encode('/example.org/foo bar', X),
   X == '/example.org/foo%20bar'.
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 2') :-
   uri_encode(X, '/example.org/foo%20bar'),
   X == '/example.org/foo bar'.
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 3') :-
   catch(uri_encode(123, _), error(E, _), true),
   E == type_error(atom, 123).
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 4') :-
   uri_encode('/example.org/foäo?baör#baüz', X),
   X == '/example.org/fo%C3%A4o?ba%C3%B6r#ba%C3%BCz'.
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 5') :-
   uri_encode(X, '/example.org/fo%C3%A4o?ba%C3%B6r#ba%C3%BCz'),
   X == '/example.org/foäo?baör#baüz'.
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 6') :-
   uri_encode('a𝄞b€c', X),
   X = 'a%F0%9D%84%9Eb%E2%82%ACc'.
runner:case(uri_encode, 2, system_connect, 'XLOG 1.1.3, XLOG 7') :-
   uri_encode(X, 'a%F0%9D%84%9Eb%E2%82%ACc'),
   X = 'a𝄞b€c'.

/* make_link(S, P, H, U) */

runner:ref(make_link, 4, system_connect, 'XLOG 1.1.4').
runner:case(make_link, 4, system_connect, 'XLOG 1.1.4, XLOG 1') :-
   make_link(S, P, H, '/example.org/foo?jack=jill&jack=jeff#bar'),
   S == '/example.org/foo', P == [jack-jill, jack-jeff], H == bar.
runner:case(make_link, 4, system_connect, 'XLOG 1.1.4, XLOG 2') :-
   make_link('/example.org/foo', [jack-jill, jack-jeff], bar, U),
   U == '/example.org/foo?jack=jill&jack=jeff#bar'.
runner:case(make_link, 4, system_connect, 'XLOG 1.1.4, XLOG 3') :-
   make_link(S, P, H, 'foo?bar=%2520'),
   S == foo, P == [bar-'%20'], H == ''.
runner:case(make_link, 4, system_connect, 'XLOG 1.1.4, XLOG 4') :-
   make_link(foo, [bar-'%20'], '', X),
   X == 'foo?bar=%2520'.
runner:case(make_link, 4, system_connect, 'XLOG 1.1.4, XLOG 5') :-
   catch(make_link(_, _, _, _), error(E, _), true),
   E == instantiation_error.
runner:case(make_link, 4, system_connect, 'XLOG 1.1.4, XLOG 6') :-
   catch(make_link(_, 123, _, _), error(E, _), true),
   E == type_error(list, 123).

/* uri_puny(S, P) */

runner:ref(uri_puny, 2, system_connect, 'XLOG 1.1.5').
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 1') :-
   uri_puny('http://zürich.ch/robots.txt', X),
   X == 'http://xn--zrich-kva.ch/robots.txt'.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 2') :-
   uri_puny(X, 'http://xn--zrich-kva.ch/robots.txt'),
   X == 'http://zürich.ch/robots.txt'.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 3') :-
   uri_puny('mailto:foo@zürich.ch', X),
   X == 'mailto:foo@xn--zrich-kva.ch'.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 4') :-
   uri_puny(X, 'mailto:foo@xn--zrich-kva.ch'),
   X == 'mailto:foo@zürich.ch'.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 5') :-
   uri_puny('jar:http://zürich.ch/archive.jar!/entry.txt', X),
   X == 'jar:http://xn--zrich-kva.ch/archive.jar!/entry.txt'.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 6') :-
   uri_puny(X, 'jar:http://xn--zrich-kva.ch/archive.jar!/entry.txt'),
   X == 'jar:http://zürich.ch/archive.jar!/entry.txt'.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 7') :-
   catch(uri_puny(_, _), error(E, _), true),
   E == instantiation_error.
runner:case(uri_puny, 2, system_connect, 'XLOG 1.1.5, XLOG 8') :-
   catch(uri_puny(123, _), error(E, _), true),
   E == type_error(atom, 123).

/* make_authority(U, H, P, A) */

runner:ref(make_authority, 4, system_connect, 'XLOG 1.1.6').
runner:case(make_authority, 4, system_connect, 'XLOG 1.1.6, XLOG 1') :-
   make_authority(X, Y, Z, 'zürich.ch:8080'),
   X == '', Y == 'zürich.ch', Z == 8080.
runner:case(make_authority, 4, system_connect, 'XLOG 1.1.6, XLOG 2') :-
   make_authority('', 'zürich.ch', 8080, X),
   X == 'zürich.ch:8080'.
runner:case(make_authority, 4, system_connect, 'XLOG 1.1.6, XLOG 3') :-
   make_authority(X, Y, Z, 'user:password@zürich.ch'),
   X == 'user:password', Y == 'zürich.ch', Z == -1.
runner:case(make_authority, 4, system_connect, 'XLOG 1.1.6, XLOG 4') :-
   make_authority('user:password', 'zürich.ch', -1, X),
   X == 'user:password@zürich.ch'.
runner:case(make_authority, 4, system_connect, 'XLOG 1.1.6, XLOG 6') :-
   catch(make_authority(_, _, _, 123), error(E, _), true),
   E == type_error(atom, 123).
runner:case(make_authority, 4, system_connect, 'XLOG 1.1.6, XLOG 7') :-
   catch(make_authority(_, 123, _, _), error(E, _), true),
   E == instantiation_error.
