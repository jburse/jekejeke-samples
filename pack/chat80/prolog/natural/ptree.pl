/**
 * Prolog text ptree from Chat80 as a module.
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

/**
 * Obtained rights comment in Prolog text and text from LICENSE file:
 *
 * @(#)ptree.pl	24.1 2/24/88
 *
 * Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,
 *
 * All Rights Reserved
 *
 * This program may be used, copied, altered or included in other programs
 * only for academic purposes and provided that the authorship of the
 * initial program is acknowledged. Use for commercial purposes without the
 * previous written agreement of the authors is forbidden.
 */

:- if(current_prolog_flag(dialect, jekejeke)).

:- package(library(natural)).

:- endif.

:- module(ptree, [print_tree/1, simplify/2]).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(edinburgh)); true.
:- use_module('../database/chatops').

/* Print term as a tree */

% :- mode print_tree(+).
% :- mode pt(+,+).
% :- mode pl(+,+).
% :- mode as_is(+).

print_tree(T) :-
   numbervars(T, 1, _),
   pt(T, 0), nl, fail.
print_tree(_).

pt(A, I) :-
   as_is(A), !,
   tab(I), write(A), nl.
pt([T|Ts], I) :- !,
   pt(T, I),
   pl(Ts, I).
pt(T, I) :- !,
   T =.. [F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl(As, I0).

pl([], _) :- !.
pl([A|As], I) :- !,
   pt(A, I),
   pl(As, I).

as_is(A) :- atomic(A), !.
as_is('$VAR'(_)) :- !.
as_is(X) :-
   quote(X).

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_, _, _, _, _)).
quote(wh(_)).
quote(name(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_, _)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).

% :- mode simplify(+,?),
%         simplify(+,?,?),
%         simplify_not(+,?),
%         revand(+,+,?),
%         report(?,+,+,+),
%         report_item(+,?).

simplify(C, (P :- R)) :- !,
   unequalise(C, (P :- Q)),
   simplify(Q, R, true).

simplify(setof(X, P0, S), R, R0) :- !,
   simplify(P0, P, true),
   revand(R0, setof(X, P, S), R).
simplify((P, Q), R, R0) :-
   simplify(Q, R1, R0),
   simplify(P, R, R1).
simplify(true, R, R) :- !.
simplify(X^P0, R, R0) :- !,
   simplify(P0, P, true),
   revand(R0, X^P, R).
simplify(numberof(X, P0, Y), R, R0) :- !,
   simplify(P0, P, true),
   revand(R0, numberof(X, P, Y), R).
simplify(\+ P0, R, R0) :- !,
   simplify(P0, P1, true),
   simplify_not(P1, P),
   revand(R0, P, R).
simplify(P, R, R0) :-
   revand(R0, P, R).

simplify_not(\+ P, P) :- !.
simplify_not(P, \+ P).

revand(true, P, P) :- !.
revand(P, true, P) :- !.
revand(P, Q, (Q, P)).

unequalise(C0, C) :-
   numbervars(C0, 1, N),
   functor(V, v, N),
   functor(M, v, N),
   inv_map(C0, V, M, C).

inv_map('$VAR'(I), V, _, X) :- !,
   arg(I, V, X).
inv_map(A = B, V, M, T) :- !,
   drop_eq(A, B, V, M, T).
inv_map(X^P0, V, M, P) :- !,
   inv_map(P0, V, M, P1),
   exquant(X, V, M, P1, P).
inv_map(A, _, _, A) :- atomic(A), !.
inv_map(T, V, M, R) :-
   functor(T, F, K),
   functor(R, F, K),
   inv_map_list(K, T, V, M, R).

inv_map_list(0, _, _, _, _) :- !.
inv_map_list(K0, T, V, M, R) :-
   arg(K0, T, A),
   arg(K0, R, B),
   inv_map(A, V, M, B),
   K is K0-1,
   inv_map_list(K, T, V, M, R).

drop_eq('$VAR'(I), '$VAR'(J), V, M, true) :- !,
   (  I =\= J, !,
      irev(I, J, K, L),
      arg(K, M, L),
      arg(K, V, X),
      arg(L, V, X)
   ;  true).
drop_eq('$VAR'(I), T, V, M, true) :- !,
   deref(I, M, J),
   arg(J, V, T),
   arg(J, M, 0).
drop_eq(T, '$VAR'(I), V, M, true) :- !,
   deref(I, M, J),
   arg(J, V, T),
   arg(J, M, 0).
drop_eq(X, Y, _, _, X = Y).

deref(I, M, J) :-
   arg(I, M, X),
   (  var(X), !, I = J
   ;  deref(X, M, J)).

exquant('$VAR'(I), V, M, P0, P) :-
   arg(I, M, U),
   (  var(U), !,
      arg(I, V, X),
      P = X^P0
   ;  P = P0).

irev(I, J, I, J) :- I > J, !.
irev(I, J, J, I).
