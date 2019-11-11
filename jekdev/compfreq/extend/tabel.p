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

/**********************************************************/
/* Tabling                                                */
/**********************************************************/

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
table_test12(X, [Y|Z]) :- table_test122(Z, X, Y).

:- table table_test122/3.
table_test122(_, X, X).
table_test122([Y|Z], X, _) :- table_test122(Z, X, Y).

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

/* resync and error */
:- table table_test16/1.
table_test16(2).
table_test16(1).
table_test16(3).

:- table table_test17/1.
table_test17(X) :- X is 1/0.

runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 11a') :-
   table_test16(X), !,
   X == 1.
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 11b') :-
   findall(X, table_test16(X), L),
   L == [1, 2, 3].
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 12a') :-
   catch(table_test17(_), error(E, _), true),
   E == evaluation_error(zero_divisor).
runner:case(table, 1, extend_tabel, 'XLOG 2.6.1, XLOG 12b') :-
   catch(table_test17(_), error(E, _), true),
   E == evaluation_error(zero_divisor).

/**********************************************************/
/* Eager Tabling                                          */
/**********************************************************/

/* table/1 plus as/2 */

/* fibonacci */
:- table table_test21/2 as [type(hash), eager(true)].
table_test21(0, 0) :- !.
table_test21(1, 1) :- !.
table_test21(N, X) :-
   H is N-1,
   table_test21(H, Y),
   J is H-1,
   table_test21(J, Z),
   X is Y+Z.

/* member */
:- table table_test22/2 as [type(hash), eager(true)].
table_test22(X, [Y|Z]) :- table_test222(Z, X, Y).

:- table table_test222/3 as [type(hash), eager(true)].
table_test222(_, X, X).
table_test222([Y|Z], X, _) :- table_test222(Z, X, Y).

runner:ref(table_as, 2, extend_tabel, 'XLOG 2.6.2').
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 1') :-
   table_test21(10, X), X == 55.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 2') :-
   \+ table_test22(3, [1, 2, 1]).
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 3a') :-
   findall(X, table_test22(X, [1, 2, 1]), [X|_]),
   X == 1.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 3b') :-
   findall(X, table_test22(X, [1, 2, 1]), [_, X|_]),
   X == 2.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 3c') :-
   findall(X, table_test22(X, [1, 2, 1]), [_, _]).
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 4') :-
   findall(X, table_test22(X, [A, _, A]), [_, _]).

/* resync and error */
:- table table_test26/1 as [type(hash), eager(true)].
table_test26(2).
table_test26(1).
table_test26(3).

:- table table_test27/1 as [type(hash), eager(true)].
table_test27(X) :- X is 1/0.

runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 11a') :-
   table_test26(X), !,
   X == 2.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 11b') :-
   findall(X, table_test26(X), L),
   L == [2, 1, 3].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 12a') :-
   catch(table_test27(_), error(E, _), true),
   E == evaluation_error(zero_divisor).
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.2, XLOG 12b') :-
   catch(table_test27(_), error(E, _), true),
   E == evaluation_error(zero_divisor).

/**********************************************************/
/* Tabling Mode Directed                                  */
/**********************************************************/

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

/**********************************************************/
/* Eager Tabling Mode Directed                            */
/**********************************************************/

/* table/1 plus as/2, aggregates */

:- table table_test41(sum) as [type(hash), eager(true)].
table_test41(X) :- between(1, 10, X).

:- table table_test42(sum, max) as [type(hash), eager(true)].
table_test42(X, X) :- between(1, 10, X).

:- table table_test43(first(@<)) as [type(hash), eager(true)].
table_test43(X) :- member(X, [goedel, escher, bach]).

:- table table_test44(_, sum) as [type(hash), eager(true)].
table_test44(Y, X) :- (Y = 2; Y = 1; Y = 2), between(1, 3, X).

:- table table_test45(_, _, sum, max) as [type(hash), eager(true)].
table_test45([A, B], Y, X, X) :- (Y = A; Y = B; Y = A), between(1, 3, X).

runner:ref(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4').
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 1') :-
   (table_test41(_), fail; true),
   table_test41(S), S == 55.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 2') :-
   (table_test42(_, _), fail; true),
   table_test42(S, T), S == 55, T == 10.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 3') :-
   (table_test43(_), fail; true),
   table_test43(S), S == bach.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 4a') :-
   (table_test44(1, _), fail; true),
   table_test44(1, S), S == 6.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 4b') :-
   (table_test44(2, _), fail; true),
   table_test44(2, S), S == 12.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 5a') :-
   (table_test45(_, _, _, _), fail; true),
   table_test45([_, B], Y, S, T), Y == B, S == 6, T == 3.
runner:case(table_aggr_as, 2, extend_tabel, 'XLOG 2.6.4, XLOG 5b') :-
   table_test45([A, _], Y, S, T), Y == A, S == 12, T == 3.

/**********************************************************/
/* Table Access & Modify                                  */
/**********************************************************/

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
