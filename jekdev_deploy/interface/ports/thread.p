/**
 * Prolog code for the sampling thread.
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
:- use_package(foreign(jekpro/tools/call)).

:- callable_property(here, sys_context(C)),
   set_source_property(C, sys_notrace).

:- foreign(start_sampler/1, 'ThreadAPI', startSampler('Interpreter')).
:- foreign(stop_sampler/1, 'ThreadAPI', stopSampler('Object')).

:- dynamic handle/1.

start_sampler :-
   start_sampler(X),
   assertz(handle(X)).

stop_sampler :-
   retract(handle(X)),
   stop_sampler(X).

