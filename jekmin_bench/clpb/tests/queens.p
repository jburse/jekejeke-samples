/**
 * CLP(B) test queens via CLP(FD) encoding.
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
;  use_module(library(clpb)).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpb)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(basic/lists)); true.

:- ensure_loaded('../mukai/finsat.p').

% queens(-Binaries)
queens(X) :-
   queens(7, X),
   term_variables(X, L),
   labeling(L).

% queens(+Integer, -List)
queens(N, X) :-
   length(X, N),
   X ins 1..N,
   noattack_list(X),
   all_different(X).

% noattack_list(+List)
noattack_list([]).
noattack_list([X|Y]) :-
   noattack_from(Y, X, 1),
   noattack_list(Y).

% noattack_from(+List, +Variable, +Integer)
noattack_from([], _, _).
noattack_from([Y|Z], X, N) :-
   X+N #\= Y,
   Y+N #\= X,
   M is N+1,
   noattack_from(Z, X, M).

