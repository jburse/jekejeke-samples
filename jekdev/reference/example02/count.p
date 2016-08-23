/**
 * Prolog code for the port statistics without call-site information.
 *
 * The following data will be gathered:
 *    count(Fun, Arity, CallExitRedoFail).
 *
 * Copyright 2011-2015, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */

% count(Fun, Arity, CallExitRedoFail)
:- dynamic count/3.

% remove_count
remove_count :-
   retract(count(_, _, _)), fail.
remove_count.

% add_count(+CallExitRedoFail, +CallExitRedoFail, -CallExitRedoFail)
add_count(A-B-C-D, E-F-G-H, R-S-T-U) :-
   R is A + E,
   S is B + F,
   T is C + G,
   U is D + H.

% update_count(+Fun, +Arity, +CallExitRedoFail)
update_count(F, A, D) :-
   retract(count(F, A, R)), !,
   add_count(R, D, S),
   assertz(count(F, A, S)).
update_count(F, A, D) :-
   assertz(count(F, A, D)).

% get_delta(+Port, -CallExitRedoFail)
get_delta(call, 1-0-0-0).
get_delta(exit, 0-1-0-0).
get_delta(redo, 0-0-1-0).
get_delta(fail, 0-0-0-1).

% goal_tracing(+Port, +Frame)
:- multifile goal_tracing/2.
goal_tracing(P, Q) :-
   frame_property(Q, sys_call_goal(G)),
   functor(G, F, A),
   get_delta(P, D),
   update_count(F, A, D).

% show
show :-
   write('Pred\tCall\tExit\tRedo\tFail'), nl,
   count(F, A, R-S-T-U),
   write(F/A),
   write('\t'),
   write(R),
   write('\t'),
   write(S),
   write('\t'),
   write(T),
   write('\t'),
   write(U), nl, fail.
show.

% reset
reset :- remove_count.
