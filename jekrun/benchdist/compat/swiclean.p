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

/**********************************************************/
/* Clean Thread                                           */
/**********************************************************/

% sys_clean_thread(+Goal)
:- meta_predicate sys_clean_thread(0).
sys_clean_thread(G) :-
   setup_call_cleanup(
       sys_thread_init(G, T),
       nondet,
       sys_thread_fini(T)).

% sys_thread_init(+Goal, -Thread)
:- meta_predicate sys_thread_init(0, ?).
sys_thread_init(G, T) :-
   thread_create(G, T, [detach(false)]).

% sys_thread_fini(+Thread)
sys_thread_fini(T) :-
   catch(thread_signal(T,
      throw(error(system_error(user_close), _))), _, true),
   thread_join(T, _).

/**********************************************************/
/* Non-Det Utilities                                      */
/**********************************************************/

% nondet
nondet.
nondet :- fail.
