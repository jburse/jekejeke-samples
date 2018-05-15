/**
 * Prolog code for the module notation test cases.
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

:- use_module(library(inspection/notation)).

runner:ref(sys_atom_slash, 2, system_notation, 'XLOG 2.1').
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 1') :-
   catch(sys_atom_slash(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 2') :-
   sys_atom_slash(X, basic/lists),
   X == 'jekpro.frequent.basic.lists'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 3') :-
   sys_atom_slash('jekpro.frequent.basic.lists', X),
   X == basic/lists.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 4') :-
   sys_atom_slash(X, foo/bar),
   X == 'user$foo$bar'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 5') :-
   sys_atom_slash('user$foo$bar', X),
   X == foo/bar.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 6') :-
   catch(sys_atom_slash(foo/bar, _), error(E,_), true),
   E == type_error(atom,foo/bar).
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 2.1, XLOG 7') :-
   catch(sys_atom_slash(_, {123}), error(E,_), true),
   E == domain_error(module,{123}).

runner:ref(sys_callable_colon, 2, system_notation, 'XLOG 2.2').
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 1') :-
   sys_callable_colon(X, Y),
   Y == X.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 2') :-
   sys_callable_colon(X, basic/lists:member(A,B)),
   X == 'jekpro.frequent.basic.lists\bmember'(A,B).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 3') :-
   sys_callable_colon('jekpro.frequent.basic.lists\bmember'(A,B), X),
   X == basic/lists:member(A,B).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 4') :-
   sys_callable_colon(X, foo/bar:baz),
   X == 'user$foo$bar\bbaz'.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 5') :-
   sys_callable_colon('user$foo$bar\bbaz', X),
   X == foo/bar:baz.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 6') :-
   sys_callable_colon(foo/bar, X),
   X == foo/bar.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 7') :-
   catch(sys_callable_colon(_, basic/lists:_), error(E,_), true),
   E == instantiation_error.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 2.2, XLOG 8') :-
   catch(sys_callable_colon(_, foo/bar:123), error(E,_), true),
   E == type_error(callable,123).

runner:ref(sys_indicator_colon, 2, system_notation, 'XLOG 2.3').
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 1') :-
   catch(sys_indicator_colon(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 2') :-
   sys_indicator_colon(X, basic/lists:member/2),
   X == 'jekpro.frequent.basic.lists\bmember'/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 3') :-
   sys_indicator_colon('jekpro.frequent.basic.lists\bmember'/2, X),
   X == basic/lists:member/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 4') :-
   sys_indicator_colon(X, foo/bar:baz/0),
   X == 'user$foo$bar\bbaz'/0.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 5') :-
   sys_indicator_colon('user$foo$bar\bbaz'/0, X),
   X == foo/bar:baz/0.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 6') :-
   catch(sys_indicator_colon(foo/bar, _), error(E,_), true),
   E == type_error(integer,bar).
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 2.3, XLOG 7') :-
   catch(sys_indicator_colon(_, {123}), error(E,_), true),
   E == type_error(predicate_indicator,{123}).
