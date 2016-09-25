/**
 * Prolog code for the 9x9-queens benchmark.
 *
 * Originally conceived in by Max Bezzel for the 8x8 checker board.
 * Used by Edsger Dijkstra to illustrate the
 * depth-first backtracking search algorithm.
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

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

% queens
queens :-
   dataqueens(X),
   search(X, [], _).

% dataqueens(-List)
dataqueens([1,2,3,4,5,6,7,8,9]).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

% rqueens
rqueens :-
   rdataqueens(X),
   search(X, [], _).

% rdataqueens(-List)
rdataqueens([1,2,3,4,5,6,7]).

/*****************************************************************/
/* The Solver                                                    */
/*****************************************************************/

% nodiag(+List, +Integer, +Integer)
nodiag([], _, _).
nodiag([N|L], B, D) :-
   D =\= N-B,
   D =\= B-N,
   D1 is D+1,
   nodiag(L, B, D1).

% qdelete(+List, -Integer, +Integer, -List)
qdelete(L, A, A, L).
qdelete([H|T], X, A, [A|R]) :-
   qdelete(T, X, H, R).

% search(+List, +List, -List)
search([], _, []).
search([H|T], History, [Q|M]) :-
   qdelete(T, Q, H, L1),
   nodiag(History, Q, 1),
   search(L1, [Q|History], M).
