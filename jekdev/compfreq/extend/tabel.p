/**
 * Prolog code for the tabling test cases.
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

:- use_module(library(advanced/tabling)).
:- use_module(library(advanced/arith)).
:- use_module(library(basic/lists)).

/* table/1, no aggregates */

/* fibonacci */
:- table table_test11/2.
table_test11(0, 0) :- !.
table_test11(1, 1) :- !.
table_test11(N, X) :-
   H is N-1,
   table_test11(H, Y),
   J is H-1,
   table_test11(J, Z),
   X is Y+Z.

:- table table_test12/2.
table_test12(X, [X|_]).
table_test12(X, [_|Y]) :- table_test12(X, Y).

runner:ref(table, 1, extend_tabel, 'XLOG 2.6.1').
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 1') :-
   table_test11(10, X), X == 55.
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 2') :-
   \+ table_test12(3, [1, 2, 1]).
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 3a') :-
   findall(X, table_test12(X, [1, 2, 1]), [X|_]),
   X == 1.
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 3b') :-
   findall(X, table_test12(X, [1, 2, 1]), [_, X|_]),
   X == 2.
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 3c') :-
   findall(X, table_test12(X, [1, 2, 1]), [_, _]).
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 4') :-
   findall(X, table_test12(X, [A, _, A]), [_, _]).

/* table/1, aggregates */

:- table table_test21(sum).
table_test21(X) :- between(1, 10, X).

:- table table_test22(sum, max).
table_test22(X, X) :- between(1, 10, X).

:- table table_test23(first(@<)).
table_test23(X) :- member(X, [goedel, escher, bach]).

:- table table_test24(_, sum).
table_test24(Y, X) :- (Y = 2; Y = 1; Y = 2), between(1, 3, X).

:- table table_test25(_, _, sum, max).
table_test25([A, B], Y, X, X) :- (Y = A; Y = B; Y = A), between(1, 3, X).

runner:ref(table_aggr, 1, extend_tabel, 'XLOG 2.6.2').
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 1') :-
   table_test21(S), S == 55.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 2') :-
   table_test22(S, T), S == 55, T == 10.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 3') :-
   table_test23(S), S == bach.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 4a') :-
   table_test24(1, S), S == 6.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 4b') :-
   table_test24(2, S), S == 12.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 5a') :-
   table_test25([_, B], Y, S, T), Y == B, S == 6, T == 3.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.2, XLOG 5b') :-
   table_test25([A, _], Y, S, T), Y == A, S == 12, T == 3.

/* current_table/2 */

runner:ref(current_table, 2, extend_tabel, 'XLOG 2.6.3').
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.3, XLOG 1') :-
   \+ current_table(foo, _).
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.3, XLOG 2') :-
   current_table(table_test11(4, _), _).
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.3, XLOG 3') :-
   \+ current_table(table_test11(12, _), _).

/* retract_table/1 */

runner:ref(retract_table, 1, extend_tabel, 'XLOG 2.6.4').
runner:case(retract_table, 1, extend_tabel, 'XLOG 2.6.4, XLOG 1') :-
   retract_table(table_test11(4, _)),
   \+ current_table(table_test11(4, _), _).
runner:case(retract_table, 1, extend_tabel, 'XLOG 2.6.4, XLOG 2') :-
   table_test11(10, _),
   \+ current_table(table_test11(4, _), _).

/* retractall_table/1 */

runner:ref(retractall_table, 1, extend_tabel, 'XLOG 2.6.5').
runner:case(retractall_table, 1, extend_tabel, 'XLOG 2.6.5, XLOG 1') :-
   retractall_table(table_test11(_, _)),
   \+ current_table(table_test11(6, _), _).
runner:case(retractall_table, 1, extend_tabel, 'XLOG 2.6.5, XLOG 2') :-
   table_test11(10, _),
   current_table(table_test11(6, _), _).
