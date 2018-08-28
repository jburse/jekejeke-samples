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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- package(library(system)).
:- module(proxy, []).

:- public runner:ref/4.
:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- public runner:case/4.
:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- begin_module(alpha).
:- reexport(foreign(java/util/'Comparator')).
:- end_module.

:- begin_module(beta).
:- reexport(library(proxy/alpha)).
:- end_module.

:- begin_module(gamma).
:- reexport(foreign(jekpro/tools/proxy/'InterfaceSlots')).
:- end_module.

:- use_module(library(basic/proxy)).

runner:ref(sys_new_instance, 2, system_proxy, 'XLOG 4.1').
runner:case(sys_new_instance, 2, system_proxy, 'XLOG 4.1, XLOG 1 Error') :-
   catch(sys_new_instance(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_new_instance, 2, system_proxy, 'XLOG 4.1, XLOG 2 Java') :-
   sys_new_instance(alpha, X),
   reference(X).
runner:case(sys_new_instance, 2, system_proxy, 'XLOG 4.1, XLOG 3 Error') :-
   catch(sys_new_instance(gamma, _), error(E,_), true),
   E = existence_error(proxy,_).

runner:ref(sys_new_instance, 3, system_proxy, 'XLOG 4.2').
runner:case(sys_new_instance, 3, system_proxy, 'XLOG 4.2, XLOG 1 Error') :-
   catch(sys_new_instance(_, 7, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_new_instance, 3, system_proxy, 'XLOG 4.2, XLOG 2 Error') :-
   catch(sys_new_instance(alpha, 7, _), error(E,_), true),
   E = existence_error(proxy,_).
runner:case(sys_new_instance, 3, system_proxy, 'XLOG 4.2, XLOG 3 Java') :-
   sys_new_instance(gamma, 7, X),
   reference(X).

runner:ref(sys_assignable_from, 2, system_proxy, 'XLOG 4.3').
runner:case(sys_assignable_from, 2, system_proxy, 'XLOG 4.3, XLOG 1 Error') :-
   catch(sys_assignable_from(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_assignable_from, 2, system_proxy, 'XLOG 4.3, XLOG 2 Prolog') :-
   sys_assignable_from(alpha, beta).
runner:case(sys_assignable_from, 2, system_proxy, 'XLOG 4.3, XLOG 3 Prolog') :-
   \+ sys_assignable_from(gamma, beta).
runner:case(sys_assignable_from, 2, system_proxy, 'XLOG 4.3, XLOG 4 Java') :-
   sys_assignable_from(java/util/'Comparator', alpha).
runner:case(sys_assignable_from, 2, system_proxy, 'XLOG 4.3, XLOG 5 Java') :-
   \+ sys_assignable_from(java/util/'Iterator', alpha).

runner:ref(sys_instance_of, 2, system_proxy, 'XLOG 4.4').
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 4.4, XLOG 1 Error') :-
   catch(sys_instance_of(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 4.4, XLOG 2 Prolog') :-
   sys_instance_of(beta(3,7), alpha).
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 4.4, XLOG 3 Prolog') :-
   \+ sys_instance_of(beta(3,7), gamma).
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 4.4, XLOG 4 Java') :-
   sys_new_instance(alpha, X),
   sys_instance_of(X, java/util/'Comparator').
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 4.4, XLOG 5 Java') :-
   sys_new_instance(alpha, X),
   \+ sys_instance_of(X, java/util/'Iterator').
