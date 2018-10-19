/**
 * Prolog code for the port statistics with call-site information.
 *
 * The following data will be gathered:
 *    count_predicate(Fun, Arity, CallExitRedoFail).
 *    count_source(Fun, Arity, Origin, Line, CallExitRedoFail).
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

:- use_module(library(inspection/provable)).

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
   R is A+E,
   S is B+F,
   T is C+G,
   U is D+H.

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
   callable_property(G, source_file(O)),
   callable_property(G, line_no(L)), !,
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
