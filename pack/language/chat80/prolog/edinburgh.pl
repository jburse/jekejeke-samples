/**
 * Simulation of some SWI-Prolog Edinburgh predicates.
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

:- module(edinburgh, []).

:- use_module(library(misc/text)).

/**
 * get0(X):
 * The predicate calls get_code/1.
 */
% get0(-Integer)
:- public get0/1.
get0(X) :-
   get_code(X).

/**
 * get(X):
 * The predicate calls get_code/1 until a non blank character appears.
 */
% get(-Integer)
:- public get/1.
get(Y) :-
   get_code(X),
   skip_blank(X, Y).

% skip_blank(+Integer, -Integer)
:- private skip_blank/2.
skip_blank(-1, Y) :- !,
   Y = -1.
skip_blank(X, Y) :-
   code_class(X, T),
   T \== blank,
   T \== cntrl,
   T \== inval, !,
   Y = X.
skip_blank(_, Y) :-
   get_code(X),
   skip_blank(X, Y).

/**
 * name(T, L):
 * If L is a code list, then the predicate succeeds in T with the
 * number or atom from the code list L. Otherwise the predicate
 * succeeds in L with code list from T.
 */
% name(+-Atomic, -+List)
:- public name/2.
name(T, L) :-
   ground(L), !,
   list_to_name(L, T).
name(T, L) :-
   name_to_list(T, L).

% list_to_name(+List, -Atomic)
:- private list_to_name/2.
list_to_name([0'-,C|L], T) :-
   code_digit(C, 10, _), !,
   number_codes(T, [0'-,C|L]).
list_to_name([0'+,C|L], T) :-
   code_digit(C, 10, _), !,
   number_codes(T, [C|L]).
list_to_name([C|L], T) :-
   code_digit(C, 10, _), !,
   number_codes(T, [C|L]).
list_to_name(L, T) :-
   atom_codes(T, L).

% name_to_list(+Atomic, -List)
:- private name_to_list/2.
name_to_list(T, L) :-
   number(T), !,
   number_codes(T, L).
name_to_list(T, L) :-
   atom_codes(T, L).


