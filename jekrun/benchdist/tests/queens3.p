/**
 * CLP(FD) Prolog code for eight queens puzzle.
 * With balance/3 and setup_balance/3 test cases.
 *
 * Originally conceived in by Max Bezzel for the 8x8 checker board.
 * Used by Edsger Dijkstra to illustrate the
 * Depth-first backtracking search algorithm.
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

:- use_module(library(runtime/distributed)).
:- use_module(library(advanced/arith)).
:- use_module(library(finite/clpfd)).
:- use_module(library(basic/lists)).

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

queens :-
   cross(8, X, Y), modellabel(8, [X,Y|_]).

queens2 :-
   balance(cross(8, X, Y), modellabel(8, [X,Y|_]), 2).

queens4 :-
   balance(cross(8, X, Y), modellabel(8, [X,Y|_]), 4).

queens8 :-
   balance(cross(8, X, Y), modellabel(8, [X,Y|_]), 8).

setup :-
   model(8, [X,Y|Z]), cross(8, X, Y), label([X,Y|Z]).

setup2 :-
   setup_balance(model(8, [X,Y|Z]), cross(8, X, Y), label([X,Y|Z]), 2).

setup4 :-
   setup_balance(model(8, [X,Y|Z]), cross(8, X, Y), label([X,Y|Z]), 4).

setup8 :-
   setup_balance(model(8, [X,Y|Z]), cross(8, X, Y), label([X,Y|Z]), 8).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

rqueens :-
   cross(4, X, Y), modellabel(4, [X,Y|_]).

rqueens2 :-
   balance(cross(4, X, Y), modellabel(4, [X,Y|_]), 2).

rqueens4 :-
   balance(cross(4, X, Y), modellabel(4, [X,Y|_]), 4).

rqueens8 :-
   balance(cross(4, X, Y), modellabel(4, [X,Y|_]), 8).

rsetup :-
   model(4, [X,Y|Z]), cross(4, X, Y), label([X,Y|Z]).

rsetup2 :-
   setup_balance(model(4, [X,Y|Z]), cross(4, X, Y), label([X,Y|Z]), 2).

rsetup4 :-
   setup_balance(model(4, [X,Y|Z]), cross(4, X, Y), label([X,Y|Z]), 4).

rsetup8 :-
   setup_balance(model(4, [X,Y|Z]), cross(4, X, Y), label([X,Y|Z]), 8).

/*****************************************************************/
/* The Queens Problem                                            */
/*****************************************************************/

% modellabel(+Integer, -List)
modellabel(N, X) :-
   model(N, X),
   label(X).

% model(+Integer, -List)
model(N, X) :-
   length(X, N),
   X ins 1..N,
   noattack_list(X),
   all_different(X).

% cross(+Integer, -Integer, -Integer)
cross(N, X, Y) :-
   between(1, N, X),
   between(1, N, Y).

% noattack_from(+List, +Variable, +Integer)
noattack_from([], _, _).
noattack_from([Y|Z], X, N) :-
   X+N #\= Y,
   Y+N #\= X,
   M is N+1,
   noattack_from(Z, X, M).

% noattack_list(+List)
noattack_list([]).
noattack_list([X|Y]) :-
   noattack_from(Y, X, 1),
   noattack_list(Y).