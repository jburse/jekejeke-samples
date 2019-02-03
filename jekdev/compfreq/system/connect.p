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

/* make_query(N, V, R, Q) */

runner:ref(make_query, 4, system_connect, 'XLOG 1.1').
runner:case(make_query, 4, system_connect, 'XLOG 1.1, XLOG 1') :-
   make_query(foo, bar, '', Q),
   Q == 'foo=bar'.
runner:case(make_query, 4, system_connect, 'XLOG 1.1, XLOG 2') :-
   make_query(V, N, R, 'foo=bar'),
   V == foo,
   N == bar,
   R == ''.
runner:case(make_query, 4, system_connect, 'XLOG 1.1, XLOG 3') :-
   catch(make_query(_, _, _, _), error(E,_), true),
   E == instantiation_error.

/* make_uri(S, Q, H, U) */

runner:ref(make_uri, 4, system_connect, 'XLOG 1.2').
runner:case(make_uri, 4, system_connect, 'XLOG 1.2, XLOG 1') :-
   make_uri('/example.org/foo', bar, '', U),
   U == '/example.org/foo?bar'.
runner:case(make_uri, 4, system_connect, 'XLOG 1.2, XLOG 1') :-
   make_uri(S, Q, H, '/example.org/foo?bar'),
   S == '/example.org/foo',
   Q == bar,
   H == ''.
runner:case(make_uri, 4, system_connect, 'XLOG 1.2, XLOG 3') :-
   catch(make_uri(_, _, _, _), error(E,_), true),
   E == instantiation_error.
