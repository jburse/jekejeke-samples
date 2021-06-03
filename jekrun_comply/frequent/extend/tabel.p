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
:- dynamic runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(advanced/tabling)).
:- use_module(library(standard/arith)).
:- use_module(library(basic/lists)).
:- use_module(library(arithmetic/ratio)).
:- use_module(library(standard/approx)).

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

/* type(hash) */

/* table/1 plus as/2 */

/* fibonacci */
:- table table_test31/2 as [type(hash), eager(true)].
table_test31(0, 0) :- !.
table_test31(1, 1) :- !.
table_test31(N, X) :-
   H is N-1,
   table_test31(H, Y),
   J is H-1,
   table_test31(J, Z),
   X is Y+Z.

/* member */
:- table table_test32/2 as [type(hash), eager(true)].
table_test32(X, [Y|Z]) :- table_test322(Z, X, Y).

:- table table_test322/3 as [type(hash), eager(true)].
table_test322(_, X, X).
table_test322([Y|Z], X, _) :- table_test322(Z, X, Y).

runner:ref(table_as, 2, extend_tabel, 'XLOG 2.6.3').
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 1') :-
   table_test31(10, X), X == 55.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 2') :-
   \+ table_test32(3, [1, 2, 1]).
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 3a') :-
   findall(X, table_test32(X, [1, 2, 1]), [X|_]),
   X == 1.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 3b') :-
   findall(X, table_test32(X, [1, 2, 1]), [_, X|_]),
   X == 2.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 3c') :-
   findall(X, table_test32(X, [1, 2, 1]), [_, _]).
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 4') :-
   findall(X, table_test32(X, [A, _, A]), [_, _]).

/* resync and error */
:- table table_test36/1 as [type(hash), eager(true)].
table_test36(2).
table_test36(1).
table_test36(3).

:- table table_test37/1 as [type(hash), eager(true)].
table_test37(X) :- X is 1/0.

runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 11a') :-
   table_test36(X), !,
   X == 2.
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 11b') :-
   findall(X, table_test36(X), L),
   L == [2, 1, 3].
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 12a') :-
   catch(table_test37(_), error(E, _), true),
   E == evaluation_error(zero_divisor).
runner:case(table_as, 2, extend_tabel, 'XLOG 2.6.3, XLOG 12b') :-
   catch(table_test37(_), error(E, _), true),
   E == evaluation_error(zero_divisor).

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

/* type(tree) */

:- table table_test51/1.
table_test51(Y) :-
   member(Y, [a, 'A', £, '0']).

:- table table_test52/1 as [type(collator), locale(en_GB)].
table_test52(Y) :-
   member(Y, [a, 'A', £, '0']).

:- table table_test53/1 as [reverse(true)].
table_test53(Y) :-
   member(Y, [a, 'A', £, '0']).

:- table table_test54/1 as [type(collator), locale(en_GB), reverse(true)].
table_test54(Y) :-
   member(Y, [a, 'A', £, '0']).

runner:ref(table_as_tree, 2, extend_tabel, 'XLOG 2.6.5').
runner:case(table_as_tree, 2, extend_tabel, 'XLOG 2.6.5, XLOG 1') :-
   findall(Y, table_test51(Y), L),
   L == ['0', 'A', a, £].
runner:case(table_as_tree, 2, extend_tabel, 'XLOG 2.6.5, XLOG 2') :-
   findall(Y, table_test52(Y), L),
   L == [£, '0', a, 'A'].
runner:case(table_as_tree, 2, extend_tabel, 'XLOG 2.6.5, XLOG 3') :-
   findall(Y, table_test53(Y), L),
   L == [£, a, 'A', '0'].
runner:case(table_as_tree, 2, extend_tabel, 'XLOG 2.6.5, XLOG 4') :-
   findall(Y, table_test54(Y), L),
   L == ['A', a, '0', £].

:- table table_test61(_, max).
table_test61(Y, X) :-
   member(Y, [a, 'A', £, '0']), (X = 2; X = 1; X = 3).

:- table table_test62(_, max) as [type(collator), locale(en_GB)].
table_test62(Y, X) :-
   member(Y, [a, 'A', £, '0']), (X = 2; X = 1; X = 3).

:- table table_test63(_, max) as [reverse(true)].
table_test63(Y, X) :-
   member(Y, [a, 'A', £, '0']), (X = 2; X = 1; X = 3).

:- table table_test64(_, max) as [type(collator), locale(en_GB), reverse(true)].
table_test64(Y, X) :-
   member(Y, [a, 'A', £, '0']), (X = 2; X = 1; X = 3).

runner:ref(table_aggr_as_tree, 2, extend_tabel, 'XLOG 2.6.6').
runner:case(table_aggr_as_tree, 2, extend_tabel, 'XLOG 2.6.6, XLOG 1') :-
   findall(Y-N, table_test61(Y, N), R),
   R == ['0'-3, 'A'-3, a-3, £ -3].
runner:case(table_aggr_as_tree, 2, extend_tabel, 'XLOG 2.6.6, XLOG 2') :-
   findall(Y-N, table_test62(Y, N), R),
   R == [£ -3, '0'-3, a-3, 'A'-3].
runner:case(table_aggr_as_tree, 2, extend_tabel, 'XLOG 2.6.6, XLOG 3') :-
   findall(Y-N, table_test63(Y, N), R),
   R == [£ -3, a-3, 'A'-3, '0'-3].
runner:case(table_aggr_as_tree, 2, extend_tabel, 'XLOG 2.6.6, XLOG 4') :-
   findall(Y-N, table_test64(Y, N), R),
   R == ['A'-3, a-3, '0'-3, £ -3].

/* type(callback) */

:- table table_test71/1.
table_test71(Y) :-
   member(Y, [2#3, 1#2, 4#11]).

:- table table_test72/1 as [type(callback), comparator(number_compare)].
table_test72(Y) :-
   member(Y, [2#3, 1#2, 4#11]).

:- table table_test73/1 as [reverse(true)].
table_test73(Y) :-
   member(Y, [2#3, 1#2, 4#11]).

:- table table_test74/1 as [type(callback), comparator(number_compare), reverse(true)].
table_test74(Y) :-
   member(Y, [2#3, 1#2, 4#11]).

runner:ref(table_as_callback, 2, extend_tabel, 'XLOG 2.6.7').
runner:case(table_as_callback, 2, extend_tabel, 'XLOG 2.6.7, XLOG 1') :-
   findall(Y, table_test71(Y), L),
   L == [1#2, 2#3, 4#11].
runner:case(table_as_callback, 2, extend_tabel, 'XLOG 2.6.7, XLOG 2') :-
   findall(Y, table_test72(Y), L),
   L == [4#11, 1#2, 2#3].
runner:case(table_as_callback, 2, extend_tabel, 'XLOG 2.6.7, XLOG 3') :-
   findall(Y, table_test73(Y), L),
   L == [4#11, 2#3, 1#2].
runner:case(table_as_callback, 2, extend_tabel, 'XLOG 2.6.7, XLOG 4') :-
   findall(Y, table_test74(Y), L),
   L == [2#3, 1#2, 4#11].

:- table table_test81(_, max).
table_test81(Y, X) :-
   member(Y, [2#3, 1#2, 4#11]), (X = 2; X = 1; X = 3).

:- table table_test82(_, max) as [type(callback), comparator(number_compare)].
table_test82(Y, X) :-
   member(Y, [2#3, 1#2, 4#11]), (X = 2; X = 1; X = 3).

:- table table_test83(_, max) as [reverse(true)].
table_test83(Y, X) :-
   member(Y, [2#3, 1#2, 4#11]), (X = 2; X = 1; X = 3).

:- table table_test84(_, max) as [type(callback), comparator(number_compare), reverse(true)].
table_test84(Y, X) :-
   member(Y, [2#3, 1#2, 4#11]), (X = 2; X = 1; X = 3).

runner:ref(table_aggr_as_callback, 2, extend_tabel, 'XLOG 2.6.8').
runner:case(table_aggr_as_callback, 2, extend_tabel, 'XLOG 2.6.8, XLOG 1') :-
   findall(Y-N, table_test81(Y, N), R),
   R == [1#2-3, 2#3-3, 4#11-3].
runner:case(table_aggr_as_callback, 2, extend_tabel, 'XLOG 2.6.8, XLOG 2') :-
   findall(Y-N, table_test82(Y, N), R),
   R == [4#11-3, 1#2-3, 2#3-3].
runner:case(table_aggr_as_callback, 2, extend_tabel, 'XLOG 2.6.8, XLOG 3') :-
   findall(Y-N, table_test83(Y, N), R),
   R == [4#11-3, 2#3-3, 1#2-3].
runner:case(table_aggr_as_callback, 2, extend_tabel, 'XLOG 2.6.8, XLOG 4') :-
   findall(Y-N, table_test84(Y, N), R),
   R == [2#3-3, 1#2-3, 4#11-3].

/**********************************************************/
/* Table Access & Modify                                  */
/**********************************************************/

/* current_table/2 */

runner:ref(current_table, 2, extend_tabel, 'XLOG 2.6.9').
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.9, XLOG 1') :-
   \+ current_table(foo, _).
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.9, XLOG 2') :-
   current_table(table_test11(4, _), _).
runner:case(current_table, 2, extend_tabel, 'XLOG 2.6.9, XLOG 3') :-
   \+ current_table(table_test11(12, _), _).

/* retract_table/1 */

runner:ref(retract_table, 1, extend_tabel, 'XLOG 2.6.10').
runner:case(retract_table, 1, extend_tabel, 'XLOG 2.6.10, XLOG 1') :-
   retract_table(table_test11(4, _)),
   \+ current_table(table_test11(4, _), _).
runner:case(retract_table, 1, extend_tabel, 'XLOG 2.6.10, XLOG 2') :-
   table_test11(10, _),
   \+ current_table(table_test11(4, _), _).

/* retractall_table/1 */

runner:ref(retractall_table, 1, extend_tabel, 'XLOG 2.6.11').
runner:case(retractall_table, 1, extend_tabel, 'XLOG 2.6.11, XLOG 1') :-
   retractall_table(table_test11(_, _)),
   \+ current_table(table_test11(6, _), _).
runner:case(retractall_table, 1, extend_tabel, 'XLOG 2.6.11, XLOG 2') :-
   table_test11(10, _),
   current_table(table_test11(6, _), _).
