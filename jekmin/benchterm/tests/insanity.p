/**
 * Instant Insanity Puzzle
 * https://www.jaapsch.net/puzzles/insanity.htm
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

:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(term/herbrand)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(basic/lists)); true.
:- current_prolog_flag(dialect, sicstus)
-> use_module(library(lists)); true.

insanity([C1,C2,C3,C4]) :-
   faces([C1,C2,C3,C4], 2, L2),
   all_dif(L2),
   faces([C1,C2,C3,C4], 3, L3),
   all_dif(L3),
   faces([C1,C2,C3,C4], 4, L4),
   all_dif(L4),
   faces([C1,C2,C3,C4], 6, L6),
   all_dif(L6),
   cube(1, C1),
   rotate(2, C2),
   rotate(3, C3),
   rotate(4, C4).

% cube(+Integer, -List)
cube(1, [r,y,r,b,r,g]).
cube(2, [g,b,y,r,g,b]).
cube(3, [b,y,g,r,r,y]).
cube(4, [b,y,r,g,y,g]).

% rotate(+Integer, -List)
rotate(S, [X1,X2,X3,X4,X5,X6]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X3,X2,X5,X4,X6,X1]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X5,X2,X6,X4,X1,X3]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X6,X2,X1,X4,X3,X5]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X6,X1,X4,X5,X3,X2]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X4,X1,X3,X5,X2,X6]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X3,X1,X2,X5,X6,X4]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X2,X1,X6,X5,X4,X3]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X2,X6,X5,X3,X4,X1]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X5,X6,X4,X3,X1,X2]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X4,X6,X1,X3,X2,X5]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X1,X6,X2,X3,X5,X4]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X5,X4,X3,X2,X1,X6]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X3,X4,X1,X2,X6,X5]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X1,X4,X6,X2,X5,X3]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X6,X4,X5,X2,X3,X1]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X6,X5,X2,X1,X3,X4]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X2,X5,X3,X1,X4,X6]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X3,X5,X4,X1,X6,X2]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X4,X5,X6,X1,X2,X3]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X2,X3,X1,X6,X4,X5]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X1,X3,X4,X6,X5,X2]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X4,X3,X5,X6,X2,X1]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).
rotate(S, [X5,X3,X2,X6,X1,X4]) :-
   cube(S, [X1,X2,X3,X4,X5,X6]).

% faces(+List, +Integer, -List)
faces([], _, []).
faces([C|L], N, [F|R]) :-
   nth1(N, C, F),
   faces(L, N, R).

% all_dif(+List)
all_dif([]).
all_dif([X|Y]) :-
   all_dif(Y, X),
   all_dif(Y).

% all_dif(+List, +Var)
all_dif([], _).
all_dif([X|Y], Z) :-
   dif(X, Z),
   all_dif(Y, Z).

% ?- solution(L).
% L = [[r,y,r,b,r,g],[g,b,y,r,g,b],[y,g,b,y,r,r],[b,r,g,g,y,y]] ;
% No