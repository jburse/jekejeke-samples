/**
 * CLP(B) test pigeons.
 *
 * State of affair is represented as:
 *    xij <=> pigeon i is placed in hole j      i in 0..n-1, j in 0..m-1
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

:- current_prolog_flag(dialect, swi)
-> use_module(library(clpb)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpb)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(basic/lists)); true.

pigeon(X) :-
   pigeon(5, 5, X),
   term_variables(X, L),
   labeling(L).

pigeon(N, M, X) :-
   dim(N, M, X),
   placed(X),
   carries(X).

/********************************************************/
/* Model Setup                                          */
/********************************************************/

% placed(+Matrice)
placed([]).
placed([X|L]) :-
   disj(X, P),
   sat(P),
   placed(L).

% disj(+Vector, -Expr)
disj([], 0).
disj([X|L], X+P) :-
   disj(L, P).

% carries(+Matrice)
carries([]).
carries([X|L]) :-
   other(L, X),
   carries(L).

% other(+Matrice, +Vector)
other([], _).
other([X|L], Y) :-
   notboth(X, Y),
   other(L, Y).

% notboth(+Vector, +Vector)
notboth([], []).
notboth([X|L], [Y|R]) :-
   sat(~(X*Y)),
   notboth(L, R).

/********************************************************/
/* Matrix Generation                                    */
/********************************************************/

% dim(+Integer, +Integer, -Matrice)
dim(B, C, A) :-
   length(A, B),
   dims(A, C).

% dim(+Matrice, +Integer)
dims([], _).
dims([A|C], B) :-
   length(A, B),
   dims(C, B).

