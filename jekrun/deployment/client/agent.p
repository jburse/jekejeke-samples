/**
 * Prolog code for the agent.
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

:- use_package(foreign(jekpro/tools/call)).

:- use_package(foreign(jekpro/study/deployment)).


:- foreign(url_encode_utf8/2, 'Stub', urlEncodeUTF8('String')).


% fetch(+Stream,-Compound)
fetch(S, R) :- repeat,
   read(S, T),
   (  T = end_of_file, !, fail
   ;  T = exception(E),
      throw(error(
               resource_error(service_exception,E),_))
   ;  T = R).


% act(+Firstname,+Name,+AgeFrom,+AgeTo,+SalaryFrom,+SalaryTo,-Compound)
act(F, N, AF, AT, SF, ST, R) :-
   url_encode_utf8(F, F8),
   atom_concat('http://localhost:8080/deployment/service.jsp?firstname=', F8, A1),
   atom_concat(A1, '&name=', A2),
   url_encode_utf8(N, N8),
   atom_concat(A2, N8, A3),
   atom_concat(A3, '&agefrom=', A4),
   url_encode_utf8(AF, AF8),
   atom_concat(A4, AF8, A5),
   atom_concat(A5, '&ageto=', A6),
   url_encode_utf8(AT, AT8),
   atom_concat(A6, AT8, A7),
   atom_concat(A7, '&salaryfrom=', A8),
   url_encode_utf8(SF, SF8),
   atom_concat(A8, SF8, A9),
   atom_concat(A9, '&salaryto=', A10),
   url_encode_utf8(ST, ST8),
   atom_concat(A10, ST8, A11),
   setup_call_cleanup(open(A11, read, S),
      fetch(S, R),
      close(S)).
