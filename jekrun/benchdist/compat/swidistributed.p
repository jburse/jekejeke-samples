/**
 * SWI-Prolog compatible module clean.
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- ensure_loaded(swiclean).

% balance(+Goal, +Goal, +Integer)
:- meta_predicate balance(0,0,?).
balance(G, T, N) :-
   term_variables(G, I),
   term_variables((G, T), J),
   pipe_new(N, F),
   pipe_new(N, B),
   sys_clean_thread(sys_put_all(I, G, F, N)),
   sys_clean_threads(sys_put_all(J, (sys_take_all(I, F, 1), T), B, 1), N),
   sys_take_all(J, B, N).

% setup_balance(+Goal, +Goal, +Goal, +Integer)
:- meta_predicate setup_balance(0,0,0,?).
setup_balance(S, G, T, N) :-
   term_variables(G, I),
   term_variables((S, G, T), J),
   pipe_new(N, F),
   pipe_new(N, B),
   sys_clean_thread(sys_put_all(I, G, F, N)),
   sys_clean_threads(sys_put_all(J, (S, sys_take_all(I, F, 1), T), B, 1), N),
   sys_take_all(J, B, N).

% sys_clean_threads(+Goal, +Integer)
:- meta_predicate sys_clean_threads(0,?).
sys_clean_threads(_, 0) :- !.
sys_clean_threads(G, N) :- N > 0,
   M is N-1,
   sys_clean_thread(G),
   sys_clean_threads(G, M).

/**********************************************************/
/* Pipe Utilities                                         */
/**********************************************************/

% sys_take_all(+Term, +Queue, +Integer)
sys_take_all(T, Q, N) :-
   between(1, N, _),
   sys_take_all2(T, Q).

% sys_take_all2(+Term, +Queue)
sys_take_all2(T, Q) :-
   repeat,
   pipe_take(Q, A),
   (A = the(S) -> S = T;
    A = ball(E) -> throw(E);
    !, fail).

% sys_put_all(+Term, +Goal, +Queue, +Integer)
:- meta_predicate sys_put_all(?,0,?,?).
sys_put_all(T, G, Q, N) :-
   catch(sys_put_all2(T, G, Q, N),
      E,
      (E = error(system_error(user_close), _)
      -> throw(E)
      ;  pipe_put(Q, ball(E)))).

% sys_put_all2(+Term, +Goal, +Queue, +Integer)
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

% pipe_new(+Integer, -Queue)
pipe_new(N, Q) :-
   message_queue_create(Q, [max_size(N)]).

% pipe_take(+Queue -Term)
pipe_take(Q, T) :-
   catch(thread_get_message(Q, T), _,
      throw(error(system_error(user_close), _))).

% pipe_put(+Queue +Term)
pipe_put(Q, T) :-
   catch(thread_send_message(Q, T), _,
      throw(error(system_error(user_close), _))).
