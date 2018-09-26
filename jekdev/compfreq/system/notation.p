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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- package(library(system)).
:- module(notation, []).

:- public runner:ref/4.
:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- public runner:case/4.
:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(inspection/notation)).

runner:ref(sys_atom_slash, 2, system_notation, 'XLOG 3.1').
/* traditional notation */
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 1 Error') :-
   catch(sys_atom_slash(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 2 Module') :-
   sys_atom_slash(X, basic/lists),
   X == 'jekpro.frequent.basic.lists'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 3 Module') :-
   sys_atom_slash('jekpro.frequent.basic.lists', X),
   X == basic/lists.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 4 Locale') :-
   sys_atom_slash(X, foo/bar),
   X == 'system.notation$foo$bar'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 5 Locale') :-
   sys_atom_slash('system.notation$foo$bar', X),
   X == foo/bar.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 6 Error') :-
   catch(sys_atom_slash(foo/bar, _), error(E,_), true),
   E == type_error(atom,foo/bar).
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 7 Error') :-
   catch(sys_atom_slash(_, {123}), error(E,_), true),
   E == domain_error(array,123).
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 8 Foreign') :-
   sys_atom_slash(X, 'String'),
   X == 'java.lang.String'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 9 Foreign') :-
   sys_atom_slash('java.lang.String', X),
   X == 'String'.
/* array notation */
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 10 Foreign') :-
   sys_atom_slash(X, {int}),
   X == 'int[]'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 11 Locale') :-
   sys_atom_slash(X, {integer}),
   X == 'system.notation$integer[]'.
runner:case(sys_atom_slash, 2, system_notation, 'XLOG 3.1, XLOG 12 Foreign') :-
   sys_atom_slash('int[]', X),
   X == {int}.

runner:ref(sys_callable_colon, 2, system_notation, 'XLOG 3.2').
/* traditional notation */
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 1 Pass') :-
   sys_callable_colon(X, Y),
   Y == X.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 2 Module') :-
   sys_callable_colon(X, basic/lists:member(A,B)),
   X == 'jekpro.frequent.basic.lists\bmember'(A,B).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 3 Module') :-
   sys_callable_colon('jekpro.frequent.basic.lists\bmember'(A,B), X),
   X == basic/lists:member(A,B).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 4 Locale') :-
   sys_callable_colon(X, foo/bar:baz),
   X == 'system.notation$foo$bar\bbaz'.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 5 Locale') :-
   sys_callable_colon('system.notation$foo$bar\bbaz', X),
   X == foo/bar:baz.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 6 Pass') :-
   sys_callable_colon(foo/bar, X),
   X == foo/bar.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 7 Error') :-
   catch(sys_callable_colon(_, basic/lists:_), error(E,_), true),
   E == instantiation_error.
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 8 Error') :-
   catch(sys_callable_colon(_, foo/bar:123), error(E,_), true),
   E == type_error(callable,123).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 9 Foreign') :-
   sys_callable_colon(X, 'String':length(abc,3)),
   X == 'java.lang.String\blength'(abc,3).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 10 Foreign') :-
   sys_callable_colon('java.lang.String\blength'(abc,3), X),
   X == 'String':length(abc,3).
/* array notation */
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 11 Foreign') :-
   sys_callable_colon(X, {int}:new(10,A)),
   X == 'int[]\bnew'(10,A).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 12 Locale') :-
   sys_callable_colon(X, {integer}:new(10,A)),
   X == 'system.notation$integer[]\bnew'(10,A).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 13 Foreign') :-
   sys_callable_colon('int[]\bnew'(10,A), X),
   X == {int}:new(10,A).
/* receiver notation */
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 14 Locale') :-
   sys_callable_colon(C, geometry/point(X,Y)::getx(X)),
   C == 'system.notation$geometry$point\bgetx'(geometry/point(X,Y),X).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 15 Locale') :-
   sys_callable_colon('system.notation$geometry$point\bgetx'(geometry/point(X,Y),X), C),
   C == geometry/point(X,Y)::getx(X).
runner:case(sys_callable_colon, 2, system_notation, 'XLOG 3.2, XLOG 16 Locale') :-
   sys_callable_colon('system.notation$geometry$point\bgetx'(P/point(X,Y),X), C),
   C == geometry/point:getx(P/point(X,Y),X).

runner:ref(sys_indicator_colon, 2, system_notation, 'XLOG 3.3').
/* traditional notation */
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 1 Error') :-
   catch(sys_indicator_colon(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 2 Module') :-
   sys_indicator_colon(X, basic/lists:member/2),
   X == 'jekpro.frequent.basic.lists\bmember'/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 3 Module') :-
   sys_indicator_colon('jekpro.frequent.basic.lists\bmember'/2, X),
   X == basic/lists:member/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 4 Locale') :-
   sys_indicator_colon(X, foo/bar:baz/0),
   X == 'system.notation$foo$bar\bbaz'/0.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 5 Locale') :-
   sys_indicator_colon('system.notation$foo$bar\bbaz'/0, X),
   X == foo/bar:baz/0.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 6 Error') :-
   catch(sys_indicator_colon(foo/bar, _), error(E,_), true),
   E == type_error(integer,bar).
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 7 Error') :-
   catch(sys_indicator_colon(_, {123}), error(E,_), true),
   E == type_error(predicate_indicator,{123}).
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 8 Foreign') :-
   sys_indicator_colon(X, 'String':length/2),
   X == 'java.lang.String\blength'/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 9 Foreign') :-
   sys_indicator_colon('java.lang.String\blength'/2, X),
   X == 'String':length/2.
/* array notation */
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 10 Foreign') :-
   sys_indicator_colon(X, {int}:new/2),
   X == 'int[]\bnew'/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 11 Locale') :-
   sys_indicator_colon(X, {integer}:new/2),
   X == 'system.notation$integer[]\bnew'/2.
runner:case(sys_indicator_colon, 2, system_notation, 'XLOG 3.3, XLOG 12 Foreign') :-
   sys_indicator_colon('int[]\bnew'/2, X),
   X == {int}:new/2.
