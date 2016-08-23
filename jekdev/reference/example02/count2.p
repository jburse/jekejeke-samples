/**
 * Prolog code for the port statistics with call-site information.
 *
 * The following data will be gathered:
 *    count_predicate(Fun, Arity, CallExitRedoFail).
 *    count_source(Fun, Arity, Origin, Line, CallExitRedoFail).
 *
 * Copyright 2011-2015, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */

% count_predicate(Fun, Arity, CallExitRedoFail)
:- dynamic count_predicate/3.

% count_source(Fun, Arity, Origin, Line, CallExitRedoFail)
:- dynamic count_source/5.

% remove_count_predicate
remove_count_predicate :-
   retract(count_predicate(_, _, _)), fail.
remove_count_predicate.

% remove_count_source
remove_count_source :-
   retract(count_source(_, _, _, _, _)), fail.
remove_count_source.

% add_count2(+CallExitRedoFail, +CallExitRedoFail, -CallExitRedoFail)
add_count2(A-B-C-D, E-F-G-H, R-S-T-U) :-
   R is A + E,
   S is B + F,
   T is C + G,
   U is D + H.

% update_count_predicate(+Fun, +Arity, +CallExitRedoFail)
update_count_predicate(F, A, D) :-
   retract(count_predicate(F, A, R)), !,
   add_count2(R, D, S),
   assertz(count_predicate(F, A, S)).
update_count_predicate(F, A, D) :-
   assertz(count_predicate(F, A, D)).

% update_count_source(+Fun, +Arity, +Origin, +Line, +CallExitRedoFail)
update_count_source(F, A, O, L, D) :-
   retract(count_source(F, A, O, L, R)), !,
   add_count2(R, D, S),
   assertz(count_source(F, A, O, L, S)).
update_count_source(F, A, O, L, D) :-
   assertz(count_source(F, A, O, L, D)).

% get_delta2(+Port, -CallExitRedoFail)
get_delta2(call, 1-0-0-0).
get_delta2(exit, 0-1-0-0).
get_delta2(redo, 0-0-1-0).
get_delta2(fail, 0-0-0-1).

% goal_tracing(+Port, +Frame)
:- multifile goal_tracing/2.
goal_tracing(P, Q) :-
   frame_property(Q, sys_call_goal(G)),
   functor(G, F, A),
   atom_property(F, source_file(O)),
   atom_property(F, line_no(L)), !,
   get_delta2(P, D),
   update_count_predicate(F, A, D),
   update_count_source(F, A, O, L, D).
goal_tracing(P, Q) :-
   frame_property(Q, sys_call_goal(G)),
   functor(G, F, A),
   get_delta2(P, D),
   update_count_predicate(F, A, D),
   update_count_source(F, A, '', 0, D).

% show_shortname(+Path)
show_shortname(O) :-
   source_property(O, short_name(S)), !,
   write(S),
   write('\t').
show_shortname(_) :-
   write('\t').

% show_callsite(+Fun,+Arity)
show_callsite(F, A) :-
   count_source(F, A, O, L, R-S-T-U),
   write('\t'),
   show_shortname(O),
   write(L),
   write('\t'),
   write(R),
   write('\t'),
   write(S),
   write('\t'),
   write(T),
   write('\t'),
   write(U), nl, fail.
show_callsite(_, _).

% show
show2 :-
   write('Pred\tSource\tLine\tCall\tExit\tRedo\tFail'), nl,
   count_predicate(F, A, R-S-T-U),
   write(F/A),
   write('\t'),
   write('\t'),
   write('\t'),
   write(R),
   write('\t'),
   write(S),
   write('\t'),
   write(T),
   write('\t'),
   write(U), nl,
   show_callsite(F, A), fail.
show2.

% reset
reset2 :- remove_count_predicate, remove_count_source.
