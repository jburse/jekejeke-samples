/**
 * SWI-Prolog compatible module clean.
 *
 * Copyright 2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 1.1.6 (a fast and small prolog interpreter)
 */

:- ensure_loaded(swiclean).

:- meta_predicate balance(0,0,?).
balance(G, T, N) :-
   pipe_new(N, F),
   term_variables(G, I),
   sys_clean_thread(sys_put_all(I, G, F, N)),
   pipe_new(N, B),
   term_variables((G, T), J),
   sys_clean_threads(sys_put_all(J, (sys_take_all(I, F, 1), T), B, 1), N),
   sys_take_all(J, B, N).

:- meta_predicate setup_balance(0,0,0,?).
setup_balance(S, G, T, N) :-
   pipe_new(N, F),
   term_variables(G, I),
   sys_clean_thread(sys_put_all(I, G, F, N)),
   pipe_new(N, B),
   term_variables((S, G, T), J),
   sys_clean_threads(sys_put_all(J, (S, sys_take_all(I, F, 1), T), B, 1), N),
   sys_take_all(J, B, N).

:- meta_predicate sys_clean_threads(0,?).
sys_clean_threads(_, 0) :- !.
sys_clean_threads(G, N) :- N > 0,
   sys_clean_thread(G),
   M is N-1,
   sys_clean_threads(G, M).

/**********************************************************/
/* Pipe Utilities                                         */
/**********************************************************/

sys_take_all(T, Q, N) :-
   between(1, N, _),
   sys_take_all2(T, Q).

sys_take_all2(T, Q) :-
   repeat,
   pipe_take(Q, A),
   (A = the(S) -> S = T;
    A = ball(E) -> sys_raise(E);
    !, fail).

:- meta_predicate sys_put_all(?,0,?,?).
sys_put_all(T, G, Q, N) :-
   catch(sys_put_all2(T, G, Q, N),
      E,
      (E = error(system_error(user_close), _)
      -> throw(E)
      ;  pipe_put(Q, ball(E)))).

:- meta_predicate sys_put_all2(?,0,?,?).
sys_put_all2(T, G, Q, _) :-
   G,
   pipe_put(Q, the(T)),
   fail.
sys_put_all2(_, _, Q, N) :-
   between(1, N, _),
   pipe_put(Q, no),
   fail.
sys_put_all2(_, _, _, _).

/**********************************************************/
/* Pipe API                                               */
/**********************************************************/

pipe_new(M, Q) :-
   message_queue_create(Q, [max_size(M)]).

pipe_put(Q, T) :-
   thread_send_message(Q, T).

pipe_take(Q, T) :-
   thread_get_message(Q, T).