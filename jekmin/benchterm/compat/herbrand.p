/**
 * Compatiblity library for sto/1, SWI-Prolog variant.
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

:- module(herbrand, [sto/1]).

sto(X) :-
   acyclic_term(X),
   term_variables(X, L),
   sys_ensure_stos(L).

sys_ensure_stos([X|Y]) :-
   sys_ensure_sto(X),
   sys_ensure_stos(Y).
sys_ensure_stos([]).

sys_ensure_sto(X) :-
   get_attr(X, herbrand, _), !.
sys_ensure_sto(X) :-
   put_attr(X, herbrand, X).

attr_unify_hook(X, _) :-
   sto(X).
