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

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- begin_module(alpha).
:- end_module.

:- begin_module(beta).
:- reexport(verbatim(user/alpha)).
:- end_module.

:- begin_module(gamma).
:- end_module.

runner:ref(sys_instance, 2, system_proxy, 'XLOG 3.1').
runner:case(sys_instance, 2, system_proxy, 'XLOG 3.1, XLOG 1') :-
   \+ fail.

runner:ref(sys_instance, 3, system_proxy, 'XLOG 3.2').
runner:case(sys_instance, 3, system_proxy, 'XLOG 3.2, XLOG 1') :-
   \+ fail.

runner:ref(sys_get_module, 2, system_proxy, 'XLOG 3.3').
runner:case(sys_get_module, 2, system_proxy, 'XLOG 3.3, XLOG 1') :-
   catch(sys_get_module(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_get_module, 2, system_proxy, 'XLOG 3.3, XLOG 2') :-
   sys_get_module(beta(3,7), X),
   X == beta.

runner:ref(sys_subclass_of, 2, system_proxy, 'XLOG 3.4').
runner:case(sys_subclass_of, 2, system_proxy, 'XLOG 3.4, XLOG 1') :-
   catch(sys_subclass_of(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_subclass_of, 2, system_proxy, 'XLOG 3.4, XLOG 2') :-
   sys_subclass_of(beta, alpha).
runner:case(sys_subclass_of, 2, system_proxy, 'XLOG 3.4, XLOG 3') :-
   \+ sys_subclass_of(beta, gamma).

runner:ref(sys_instance_of, 2, system_proxy, 'XLOG 3.5').
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 3.5, XLOG 1') :-
   catch(sys_instance_of(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 3.5, XLOG 2') :-
   sys_instance_of(beta(3,7), alpha).
runner:case(sys_instance_of, 2, system_proxy, 'XLOG 3.5, XLOG 3') :-
   \+ sys_instance_of(beta(3,7), gamma).
