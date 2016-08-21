/**
 * SWI-Prolog compatible module clean.
 *
 * Copyright 2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 1.1.6 (a fast and small prolog interpreter)
 */

/**********************************************************/
/* Clean Thread                                           */
/**********************************************************/

:- meta_predicate sys_clean_thread(0).
sys_clean_thread(G) :-
   setup_call_cleanup(
       sys_thread_init(G, I),
       nondet,
       sys_thread_fini(I)).

:- meta_predicate sys_thread_init(0,?).
sys_thread_init(G, I) :-
   thread_create(G, I, [detach(false)]).

sys_thread_fini(I) :-
   catch(thread_signal(I, throw(error(system_error(user_close),_))), _, true),
   thread_join(I, _).

/**********************************************************/
/* Non-Det Utilities                                      */
/**********************************************************/

nondet.
nondet :- fail.
