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

runner:ref(sys_callable, 1, system_quali, 'XLOG 3.2').
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
