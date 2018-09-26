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

:- use_module(library(system/charsio)).

runner:ref(variable_names, 1, system_connect, 'XLOG 1.1').
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, XLOG 1 Output') :-
   with_output_to(atom(R), write_term(foo(A+Roger,A+H),
                              [variable_names(['A'=A,'Roger'=Roger,'_'=H])])),
   R == 'foo(A+Roger,A+_)'.
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, XLOG 2 Output') :-
   A = Roger,
   with_output_to(atom(R), write_term(foo(A+Roger,A+H),
                              [variable_names(['A'=A,'Roger'=Roger,'_'=H])])),
   R == 'foo(Roger+Roger,Roger+_)'.
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, XLOG 3 Output') :-
   A = foo,
   with_output_to(atom(R), write_term(foo(A+Roger,A+H),
                              [variable_names(['A'=A,'Roger'=Roger,'_'=H])])),
   R == 'foo(foo+Roger,foo+_)'.
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 1 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names([_=T])])), error(E,_), true),
   E == instantiation_error.
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 65 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names(['_/*.*/'=T])])), error(E,_), true),
   E == domain_error(variable_name,'_/*.*/').
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 5 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names([x=T])])), error(E,_), true),
   E == domain_error(variable_name,x).
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 6 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names(['x+y'=T])])), error(E,_), true),
   E == domain_error(variable_name,'x+y').
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 50 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names(['))'=T])])), error(E,_), true),
   E == domain_error(variable_name,'))').
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 7 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names([7=T])])), error(E,_), true),
   E == type_error(atom,7).
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 8 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names([1+2=T])])), error(E,_), true),
   E == type_error(atom,1+2).
runner:case(variable_names, 1, system_connect, 'XLOG 1.1, ISO 9 Error') :-
   catch(with_output_to(atom(_), write_term(T, [variable_names(['$VAR'(9)=T])])), error(E,_), true),
   E == type_error(atom,'$VAR'(9)).
