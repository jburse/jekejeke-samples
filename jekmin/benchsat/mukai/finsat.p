/**
 * CLP(FD) to CLP(B) translator with reification.
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

:- op(100, xfx, ..).
:- op(700, xfx, in).
:- op(700, xfx, ins).

:- op(700, xfx, #=).
:- op(700, xfx, #\=).
:- op(700, xfx, #<).
:- op(700, xfx, #>).
:- op(700, xfx, #=<).
:- op(700, xfx, #>=).

:- op(740, yfx, #\/).
:- op(720, yfx, #/\).
:- op(700, fy, #\).
:- op(700, xfy, #^).                              %%% experiment

:- op(760, yfx, #<==>).
:- op(760, xfx, #==>).
:- op(760, xfx, #<==).

/*******************************************************************/
/* Domain Membership                                               */
/*******************************************************************/

% in(+Vec, +Range)
X in L :-
   bitin(X, L, T),
   sat(T).

% ins(+List, +Range)
V ins _ :-
   var(V),
   throw(error(instantiation_error,_)).
[] ins _ :- !.
[X|L] ins R :- !,
   X in R,
   L ins R.
X ins _ :-
   throw(error(type_error(list,X),_)).

% bitin(+Vec, +Range, -Sat)
bitin(X, N..M, ~T* ~S) :-
   numvec(N, A),
   numvec(M, B),
   bitls(X, A, T),
   bitls(B, X, S).

/*******************************************************************/
/* Equality & Inequality                                           */
/*******************************************************************/

% all_different(+List)
% SWI-Prolog like naming
% Does a validation of the list
all_different(V) :-
   var(V),
   throw(error(instantiation_error,_)).
all_different([]) :- !.
all_different([X|Y]) :- !,
   all_different(Y),
   sys_nq_list(Y, X).
all_different(X) :-
   throw(error(type_error(list,X),_)).

% sys_nq_list(+List, +Expr)
% Doesn't do a validation of the list
sys_nq_list([], _).
sys_nq_list([Y|Z], X) :-
   X #\= Y,
   sys_nq_list(Z, X).

% #=(+Vec, +Vec)
X #= Y :-
   bitexpr(X, A),
   bitexpr(Y, B),
   biteq(A, B, T),
   sat(T).

% #\=(+Vec, +Vec)
X #\= Y :-
   bitexpr(X, A),
   bitexpr(Y, B),
   biteq(A, B, T),
   sat(~T).

% #<(+Vec, +Vec)
X #< Y :-
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(A, B, T),
   sat(T).

% #>(+Vec, +Vec)
X #> Y :-
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(B, A, T),
   sat(T).

% #>=(+Vec, +Vec)
X #>= Y :-
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(A, B, T),
   sat(~T).

% #=<(+Vec, +Vec)
X #=< Y :-
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(B, A, T),
   sat(~T).

% biteq(+Vec, +Vec, -Sat)
biteq([], [], 1).
biteq([X|L], [Y|R], (X=:=Y)*H) :-
   biteq(L, R, H).

% bitls(+Vec, +Vec, -Sat)
bitls([], [], 0).
bitls([X|L], [Y|R], (X<Y)+(X=:=Y)*H) :-
   bitls(L, R, H).

% bitexpr(+Expr, -Vec)
bitexpr([X|L], R) :- !,
   R = [X|L].
bitexpr(X+Y, C) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   addvec(A, B, C, _).
bitexpr(X*Y, C) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   numvec(0, D),
   mulvec(A, B, D, C).
bitexpr(N, A) :-
   numvec(N, A).

/*******************************************************************/
/* Reified Equality & Inequality                                   */
/*******************************************************************/

% #\/(+Sat, +Sat)
X #\/ Y :-
   satexpr(X, A),
   satexpr(Y, B),
   sat(A+B).

% #/\(+Sat, +Sat)
X #/\ Y :-
   satexpr(X, A),
   satexpr(Y, B),
   sat(A*B).

% #\(+Sat)
#\ X :-
   satexpr(X, A),
   sat(~A).

% #^(+Var, +Sat)
X #^ Y :-                                         %%% experiment
   satexpr(Y, A),
   bitquant(X, A, T),
   sat(T).

% #<==>(+Sat, +Sat)
X #<==> Y :-
   satexpr(X, A),
   satexpr(Y, B),
   sat(A=:=B).

% #==>(+Sat, +Sat)
X #==> Y :-
   satexpr(X, A),
   satexpr(Y, B),
   sat(A=<B).

% #<==(+Sat, +Sat)
X #<== Y :-
   satexpr(X, A),
   satexpr(Y, B),
   sat(A>=B).

satexpr([X|L], T) :- !,
   numvec(1, B),
   biteq([X|L], B, T).
satexpr(X#=Y, T) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   biteq(A, B, T).
satexpr(X#\=Y, ~T) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   biteq(A, B, T).
satexpr(X#<Y, T) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(A, B, T).
satexpr(X#>Y, T) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(B, A, T).
satexpr(X#>=Y, ~T) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(A, B, T).
satexpr(X#=<Y, ~T) :- !,
   bitexpr(X, A),
   bitexpr(Y, B),
   bitls(B, A, T).
satexpr(X#\/Y, T+S) :- !,
   satexpr(X, T),
   satexpr(Y, S).
satexpr(X#/\Y, T*S) :- !,
   satexpr(X, T),
   satexpr(Y, S).
satexpr(#\X, ~T) :- !,
   satexpr(X, T).
satexpr(X#^Y, T) :- !,                                             %%% experiment
   satexpr(Y, A),
   bitquant(X, A, T).
satexpr(X#<==>Y, T=:=S) :- !,
   satexpr(X, T),
   satexpr(Y, S).
satexpr(X#==>Y, T=<S) :- !,
   satexpr(X, T),
   satexpr(Y, S).
satexpr(X#<==Y, T>=S) :- !,
   satexpr(X, T),
   satexpr(Y, S).
satexpr(N, T) :-
   numvec(N, A),
   numvec(1, B),
   biteq(A, B, T).

bitquant([], A, A).                               %%% experiment
bitquant([X|Y], A, X^B) :-
   var(X), !,
   bitquant(Y, A, B).
bitquant([_|Y], A, B) :-
   bitquant(Y, A, B).

/*******************************************************************/
/* Bit Arithmetic                                                  */
/*******************************************************************/

:- dynamic sys_numlen/1.

numlen(K) :-
   retract(sys_numlen(_)), !,
   assertz(sys_numlen(K)).
numlen(K) :-
   assertz(sys_numlen(K)).

% numvec(-+Integer, -+Vec)
numvec(N, L) :-
   ground(L), !,
   numvec(L, 0, N).
numvec(N, L) :-
   sys_numlen(K), !,
   numvec(K, N, L, _).
numvec(N, L) :-
   numvec(4, N, L, _).

% numvec(+List, +Integer, -Integer)
numvec([], S, S).
numvec([X|L], S, T) :-
   H is 2*S+X,
   numvec(L, H, T).

% numvec(+Integer, +Integer, -Vec, -Integer)
numvec(0, N, [], N) :- !.
numvec(K, N, [X|L], Y) :-
   J is K-1,
   numvec(J, N, L, M),
   X is M mod 2,
   Y is M div 2.

% addvec(+List, +List, -List, -Bool)
addvec([], [], [], 0).
addvec([X|L], [Y|R], [S|H], C) :-
   addvec(L, R, H, Z),
   fulladd(X, Y, Z, S, C).

% fulladd(+Bool, +Bool, +Bool, -Bool, -Bool)
fulladd(X, Y, Z, S, J+K) :-
   halfadd(X, Y, H, J),
   halfadd(H, Z, S, K).

% halfadd(+Bool, +Bool, -Bool, -Bool)
halfadd(X, Y, X#Y, X*Y).

% mulvec(+List, +List, +List, -List)
mulvec([], _, S, S).
mulvec([X|L], R, S, T) :-
   shiftvec(S, H, _),
   mulbit(R, X, J),
   addvec(H, J, K, _),
   mulvec(L, R, K, T).

% mulbit(+List, +Bool, -List)
mulbit([], _, []).
mulbit([Y|R], X, [X*Y|H]) :-
   mulbit(R, X, H).

% shiftvec(+List, -List, -Bool)
shiftvec([], [], 0).
shiftvec([X|L], [Y|R], X) :-
   shiftvec(L, R, Y).
