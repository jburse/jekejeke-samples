/**
 * SiCStus Prolog code for the test harness.
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

% ?- ensure_loaded('/Projects/Jekejeke/Prototyping/samples/jekmin/benchmark/harness/swi4.p').

uptime(T) :-
   statistics(walltime, [T|_]).

gctime(T) :-
   statistics(garbage_collection, [_,_,T|_]).

:- use_module(library(clpfd)).

:- op(700, xfx, ins).
:- op(700, xfx, in).
:- op(550, xfx, ..).

Vs ins A..B :- !,
   domain(Vs, A, B).
[V|L] ins R :-
   point_to_range(R, S),
   V in S,
   L ins R.
label(Vs) :-
   labeling([], Vs).

point_to_range(A\/B, C\/D) :- !,
   point_to_range(A, C),
   point_to_range(B, D).
point_to_range(A, A..A).

label_maximum(L, M) :-
   labeling([maximize(M)], L).

:- ensure_loaded('suite4.p').
