/**
 * CLP(FD) code for the boolean pigeon hole problem.
 *
 * Clauses can be represented as CLP(FD) as follows:
 *    x1 v .. v xn :<=> x1+..+xn #> 0.
 *    ~x := 1-x
 * State of affair is represented as:
 *    xij <=> pigeon i is placed in hole j           i in 0..n-1, j in 0..m-1
 * Clause for each pigeon that it is placed in at least one hole:
 *    xi0 v .. v xim-1       i in 0..n-1
 * Clauses for each hole that it carries maximally one pigeon:
 *    ~xij v ~xkj            i in 0..n-1, k in i+1..n-1, j in 0..m-1.
 * Should work correctly for n>=m.
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

:- current_prolog_flag(dialect, jekejeke) -> true
;  use_module(library(clpfd)).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpfd)); true.

/**********************************************************/
/* Matrice Generation                                     */
/**********************************************************/

% size(-List, +Integer)
size([], 0).
size([_|Y], N) :-
   N > 0,
   M is N-1,
   size(Y, M).

% dimension(+ListOfList, +Integer)
dimension([], _).
dimension([X|Y], N) :-
   size(X, N),
   dimension(Y, N).

% matrice(-ListOfList, +Integer, +Integer)
matrice(X, N, M) :-
   size(X, N),
   dimension(X, M).

/**********************************************************/
/* Constraint Generation                                  */
/**********************************************************/

% makesum(+List, -Sum)
makesum([X], X).
makesum([X,Y|Z], H) :-
   makesum([Y|Z], T),
   H #= X+T.

% placed(+ListOfList)
placed([]).
placed([X|Y]) :-
   makesum(X, T),
   T #> 0,
   placed(Y).

% notboth(+List, +List)
notboth([], []).
notboth([X|Y], [Z|T]) :-
   2-X-Z #> 0,
   notboth(Y, T).

% other(+ListOfList, +List)
other([], _).
other([X|Y], Z) :-
   notboth(X, Z),
   other(Y, Z).

% carries(+ListOfList)
carries([]).
carries([X|Y]) :-
   other(Y, X),
   carries(Y).

% pigeon3(+ListOfList)
pigeon3(X) :-
   matrice(X, 6, 5),
   term_variables(X, L),
   L ins 0..1,
   placed(X),
   carries(X),
   label(L).
