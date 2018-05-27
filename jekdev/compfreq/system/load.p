/**
 * Prolog code for the load test cases.
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

runner:ref(absolute_file_name, 2, system_load, 'XLOG 2.1').
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 1 Error') :-
   catch(absolute_file_name(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 2 Library') :-
   absolute_file_name(library(basic/lists), _).
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 3 Error') :-
   catch(absolute_file_name(library(basic/foo), _), error(E,_), true),
   E == existence_error(library,basic/foo).
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 4 Library') :-
   absolute_file_name(library(basic/lists), X),
   absolute_file_name(Y, X),
   Y == library(basic/lists).
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 5 Foreign') :-
   absolute_file_name(foreign('String'), _).
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 6 Error') :-
   catch(absolute_file_name(foreign('Foo'), _), error(E,_), true),
   E == existence_error(class,'Foo').
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 7 Foreign') :-
   absolute_file_name(foreign('String'), X),
   absolute_file_name(Y, X),
   Y == foreign('String').
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 8 Verbatim') :-
   absolute_file_name(verbatim(foo), _).
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 9 Verbatim') :-
   absolute_file_name(verbatim(foo), X),
   absolute_file_name(Y, X),
   Y == verbatim(foo).
runner:case(absolute_file_name, 2, system_load, 'XLOG 2.1, XLOG 10 Error') :-
   catch(absolute_file_name(foo(bar), _), error(E,_), true),
   E == type_error(path,foo(bar)).

:- sys_auto_load(foreign(java/util/'Comparator')).

:- sys_auto_load(verbatim(foo/bar)).

runner:ref(current_module, 1, system_load, 'XLOG 2.2').
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 1 Error') :-
   catch(current_module(_/lists), error(E,_), true),
   E == instantiation_error.
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 2 Error') :-
   catch(current_module(basic/123), error(E,_), true),
   E == type_error(atom,123).
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 3 Error') :-
   catch(current_module(456/lists), error(E,_), true),
   E == domain_error(package,456).
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 4 Library') :-
   current_module(basic/lists).
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 5 Library') :-
   \+ current_module(basic/foo).
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 6 Foreign') :-
   current_module(java/util/'Comparator').
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 7 Foreign') :-
   \+ current_module(java/util/'Foo').
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 8 Verbatim') :-
   current_module(foo/bar).
runner:case(current_module, 1, system_load, 'XLOG 2.2, XLOG 9 Verbatim') :-
   \+ current_module(foo/baz).
