/**
 * Prolog code for the relative port statistics.
 *
 * The following data will be gathered:
 *    count(Fun, Arity, CallExitRedoFail).
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

:- use_package(foreign(ports)).

:- callable_property(here, sys_context(C)),
   set_source_property(C, sys_notrace).

:- foreign(format_float/3, 'RelativeAPI', formatFloat('String', 'Double')).
:- foreign(nano_time/1, 'RelativeAPI', nanoTime).

% remove_count
remove_count :-
   retract(count(_, _, _)),
   fail.
remove_count.

% add_count(+CallExitRedoFail, +CallExitRedoFail, -CallExitRedoFail)
add_count(A-B-C-D, E-F-G-H, R-S-T-U) :-
   R is A+E,
   S is B+F,
   T is C+G,
   U is D+H.

% update_count(+Fun,+Arity,+CallExitRedoFail)
update_count(F, A, D) :-
   retract(count(F, A, R)), !,
   add_count(R, D, S),
   assertz(count(F, A, S)).
update_count(F, A, D) :-
   assertz(count(F, A, D)).

% get_delta(+Port,-CallExitRedoFail)
get_delta(call, 1-0-0-0).
get_delta(exit, 0-1-0-0).
get_delta(redo, 0-0-1-0).
get_delta(fail, 0-0-0-1).

% goal_tracing(+Port, +Frame)
:- public goal_tracing/2.
:- multifile goal_tracing/2.
goal_tracing(P, Q) :-
   frame_property(Q, sys_call_indicator(F, A)),
   get_delta(P, D),
   update_count(F, A, D).

% sum_counts(+List, -CallExitRedoFail)
sum_counts([], 0-0-0-0).
sum_counts([_-D|L], S) :-
   sum_counts(L, R),
   add_count(R, D, S).

% mem_counts(+Elem, +List)
mem_counts(X, [X|_]).
mem_counts(X, [_|Y]) :-
   mem_counts(X, Y).

% show
show :-
   findall(F/A-D, count(F, A, D), L),
   sum_counts(L, TR-TS-TT-TU),
   write('Pred\tCall\tExit\tRedo\tFail'), nl,
   keysort(L, M),
   mem_counts(I-(R-S-T-U), M),
   RP is R*100/TR,
   SP is S*100/TS,
   TP is T*100/TT,
   UP is U*100/TU,
   ((RP >= 0.05; SP >= 0.05; TP >= 0.05; UP >= 0.05)
-> write(I), write('\t'),
   format_float('0.0', RP, SR), write(SR), write(' %\t'),
   format_float('0.0', SP, SS), write(SS), write(' %\t'),
   format_float('0.0', TP, ST), write(ST), write(' %\t'),
   format_float('0.0', UP, SU), write(SU), write(' %'), nl; true),
   fail.
show.

% reset
reset :-
   remove_count.

/*
% show(+Time)
show(T) :-
  format_float('0.0', T, U),
  write('\tin '),
  write(U),
  write(' ms'), nl.

% time(+Goal)
% Cannot be used to show time for redo/exit.
time(X) :-
  nano_time(T1),
  X,
  nano_time(T2),
  T is (T2-T1) / 1000000,
  write(X),
  show(T).
*/