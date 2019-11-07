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

/* member */
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

/* recursive tabling */

/* Test Data for Strongly Connected Components (SCC) */
edge(11, 14).
edge(14, 15).
edge(15, 16).
edge(16, 15).
edge(16, 17).
edge(11, 12).
edge(12, 13).
edge(13, 12).

edge(21, 26).
edge(21, 22).
edge(22, 23).
edge(23, 22).
edge(24, 25).
edge(25, 26).
edge(26, 25).
edge(25, 24).
edge(26, 27).
edge(22, 27).

/* right recursive */
:- table table_test13/2.
table_test13(X, X).
table_test13(X, Y) :- edge(X, Z), table_test13(Z, Y).

/* left recursive */
:- table table_test14/2.
table_test14(X, X).
table_test14(X, Y) :- table_test14(X, Z), edge(Z, Y).

/* left/right recursive */
:- table table_test15/2.
table_test15(X, X).
table_test15(X, Y) :- edge(X, Y).
table_test15(X, Y) :- table_test15(X, Z), table_test15(Z, Y).

runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 5') :-
   findall(X, table_test13(11, X), L),
   L == [11, 12, 13, 14, 15, 16, 17].
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 6') :-
   findall(X, table_test13(21, X), L),
   L == [21, 22, 23, 24, 25, 26, 27].
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 7') :-
   findall(X, table_test14(11, X), L),
   L == [11, 12, 13, 14, 15, 16, 17].
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 8') :-
   findall(X, table_test14(21, X), L),
   L == [21, 22, 23, 24, 25, 26, 27].
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 9') :-
   findall(X, table_test15(11, X), L),
   L == [11, 12, 13, 14, 15, 16, 17].
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 10') :-
   findall(X, table_test15(21, X), L),
   L == [21, 22, 23, 24, 25, 26, 27].

/* table/1 plus as/2 */

:- table table_test21/2 as [eager(true)].
table_test21(Y, X) :- (Y = 2; Y = 1; Y = 1), (X = 1; X = Y).

runner:ref(table_as, 2, extend_tabel, 'XLOG 2.6.2').
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 1a') :-
   findall(Y-X, table_test21(Y, X), [R|_]),
   R == 2-1.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 1b') :-
   retractall_table(table_test21(_, _)),
   findall(Y-X, table_test21(Y, X), [_, R|_]),
   R == 2-2.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 1c') :-
   retractall_table(table_test21(_, _)),
   findall(Y-X, table_test21(Y, X), [_, _, R|_]),
   R == 1-1.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 1d') :-
   retractall_table(table_test21(_, _)),
   findall(Y-X, table_test21(Y, X), [_, _, _]).

/* recursive tabling */

/* right recursive */
:- table table_test22/2 as [eager(true)].
table_test22(X, X).
table_test22(X, Y) :- edge(X, Z), table_test22(Z, Y).

/* left recursive */
:- table table_test23/2 as [eager(true)].
table_test23(X, X).
table_test23(X, Y) :- table_test23(X, Z), edge(Z, Y).

/* left/right recursive */
:- table table_test24/2 as [eager(true)].
table_test24(X, X).
table_test24(X, Y) :- edge(X, Y).
table_test24(X, Y) :- table_test24(X, Z), table_test24(Z, Y).

runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 2') :-
   findall(X, table_test22(11, X), L),
   L == [11, 14, 15, 16, 17, 12, 13].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 3') :-
   findall(X, table_test22(21, X), L),
   L == [21, 26, 25, 24, 27, 22, 23].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 4') :-
   findall(X, table_test23(11, X), L),
   L == [11, 14, 12, 13, 15, 16, 17].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 5') :-
   findall(X, table_test23(21, X), L),
   L == [21, 26, 22, 23, 27, 25, 24].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 6') :-
   findall(X, table_test24(11, X), L),
   L == [11, 14, 12, 13, 15, 16, 17].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 7') :-
   findall(X, table_test24(21, X), L),
   L == [21, 26, 22, 23, 27, 25, 24].

/* table/1, aggregates */

:- table table_test31(sum).
table_test31(X) :- between(1, 10, X).

:- table table_test32(sum, max).
table_test32(X, X) :- between(1, 10, X).

:- table table_test33(first(@<)).
table_test33(X) :- member(X, [goedel, escher, bach]).

:- table table_test34(_, sum).
table_test34(Y, X) :- (Y = 2; Y = 1; Y = 2), between(1, 3, X).

:- table table_test35(_, _, sum, max).
table_test35([A, B], Y, X, X) :- (Y = A; Y = B; Y = A), between(1, 3, X).

runner:ref(table_aggr, 1, extend_tabel, 'XLOG 2.6.3').
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 1') :-
   table_test31(S), S == 55.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 2') :-
   table_test32(S, T), S == 55, T == 10.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 3') :-
   table_test33(S), S == bach.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 4a') :-
   table_test34(1, S), S == 6.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 4b') :-
   table_test34(2, S), S == 12.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 5a') :-
   table_test35([_, B], Y, S, T), Y == B, S == 6, T == 3.
runner:case(table_aggr, 1, extend_tabel, 'XLOG 2.6.3, XLOG 5b') :-
   table_test35([A, _], Y, S, T), Y == A, S == 12, T == 3.

/* table/1 plus as/2, aggregates */

:- table table_test41(max, _) as [eager(true)].
table_test41(Y, X) :- (Y = 1; Y = 1; Y = 2), (X = 1; X = Y).

runner:ref(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4').
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 1a') :-
   findall(Y-X, table_test41(Y, X), [R|_]),
   R == 1-1.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 1b') :-
   retractall_table(table_test41(_, _)),
   findall(Y-X, table_test41(Y, X), [_, R|_]),
   R == 2-1.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 1c') :-
   retractall_table(table_test41(_, _)),
   findall(Y-X, table_test41(Y, X), [_, _, R|_]),
   R == 2-2.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 1d') :-
   retractall_table(table_test41(_, _)),
   findall(Y-X, table_test41(Y, X), [_, _, _]).

/* current_table/2 */

runner:ref(current_table, 2, extend_tabel, 'XLOG 2.6.5').
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.5, XLOG 1') :-
   \+ current_table(foo, _).
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.5, XLOG 2') :-
   current_table(table_test11(4, _), _).
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.5, XLOG 3') :-
   \+ current_table(table_test11(12, _), _).

/* retract_table/1 */

runner:ref(retract_table, 1, extend_tabel, 'XLOG 2.6.6').
runner:case(retract_table, 1, extend_tabel, 'XLOG 2.6.6, XLOG 1') :-
   retract_table(table_test11(4, _)),
   \+ current_table(table_test11(4, _), _).
runner:case(retract_table, 1, extend_tabel, 'XLOG 2.6.6, XLOG 2') :-
   table_test11(10, _),
   \+ current_table(table_test11(4, _), _).

/* retractall_table/1 */

runner:ref(retractall_table, 1, extend_tabel, 'XLOG 2.6.7').
runner:case(retractall_table, 1, extend_tabel, 'XLOG 2.6.7, XLOG 1') :-
   retractall_table(table_test11(_, _)),
   \+ current_table(table_test11(6, _), _).
runner:case(retractall_table, 1, extend_tabel, 'XLOG 2.6.7, XLOG 2') :-
   table_test11(10, _),
   current_table(table_test11(6, _), _).
