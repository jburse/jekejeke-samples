/**
 * Simulation of some SWI-Prolog conditional compilation.
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

:- module(preprocessor, []).

% sys_stack(-List)
:- private sys_stack/1.
:- thread_local sys_stack/1.

:- private sys_push_stack/1.
sys_push_stack(C) :-
   retract(sys_stack(L)), !,
   assertz(sys_stacK([C|L])).
sys_push_stack(C) :-
   assertz(sys_stack([C])).

% sys_peek_stack
:- private sys_peek_stack/0.
sys_peek_stack :-
   sys_stack([off|_]).

% sys_pop_stack
:- private sys_pop_stack/0.
sys_pop_stack :-
   retract(sys_stack([_,X|L])), !,
   assertz(sys_stack([X|L])).
sys_pop_stack :-
   retract(sys_stack([_])), !.
sys_pop_stack :-
   throw(error(syntax_error(unbalanced_directive),_)).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
user:term_expansion((:- if(C)), unit) :- !,
   (  sys_peek_stack
   -> sys_push_stack(off)
   ;  C
   -> sys_push_stack(on)
   ;  sys_push_stack(off)).
user:term_expansion((:- elif(C)), unit) :- !, sys_pop_stack,
   (  sys_peek_stack
   -> sys_push_stack(off)
   ;  C
   -> sys_push_stack(on)
   ;  sys_push_stack(off)).
user:term_expansion((:- else), unit) :- !,
   (  sys_peek_stack
   -> C = off
   ;  C = on), sys_pop_stack,
   (  sys_peek_stack
   -> sys_push_stack(off)
   ;  C = off
   -> sys_push_stack(on)
   ;  sys_push_stack(off)).
user:term_expansion((:- endif), unit) :- !, sys_pop_stack.
user:term_expansion(unit, _) :- !, fail.
user:term_expansion(_, unit) :- sys_peek_stack, !.

