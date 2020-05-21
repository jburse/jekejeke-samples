/**
 * Simulation of some SWI-Prolog character predicates.
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

:- module(unicode, []).

:- use_module(library(misc/text)).
:- use_module(library(advanced/arith)).

/**
 * char_type(C, T):
 * If C is a variable, then the predicate succeeds in C with
 * every character that satisfies the type T. Otherwise the predicate
 * succeeds in T with every type that is satisfied by the character C.
 */
% char_type(+-Atom, -+Spec)
:- public char_type/2.
char_type(X, Y) :- var(X), !,
   sys_type_code(Y, H),
   char_code(X, H).
char_type(X, Y) :-
   char_code(X, H),
   sys_code_type(Y, H).

/**
 * char_type(C, T):
 * If C is a variable, then the predicate succeeds in C with
 * every code that satisfies the type T. Otherwise the predicate
 * succeeds in T with every type that is satisfied by the code C.
 */
% code_type(+-Integer, -+Spec)
:- public code_type/2.
code_type(X, T) :- var(X), !,
   sys_type_code(T, X).
code_type(X, T) :-
   sys_code_type(T, X).

% sys_code_type(-Spec, +Integer)
:- private sys_code_type/2.
sys_code_type(white, X) :-
   sys_white(X).
sys_code_type(space, X) :-
   sys_space(X).
sys_code_type(graph, X) :-
   sys_graph(X).
sys_code_type(to_lower(Y), X) :-
   code_lower(X, Y).
sys_code_type(to_upper(Y), X) :-
   code_upper(X, Y).
sys_code_type(digit(Y), X) :-
   code_digit(X, 10, Y).
sys_code_type(xdigit(Y), X) :-
   code_digit(X, 16, Y).
sys_code_type(digit, X) :-
   sys_class(X, digit).

% sys_type_code(+Spec, -Integer)
:- private sys_type_code/2.
sys_type_code(T, _) :- var(T),
   throw(error(instantiation_error, _)).
sys_type_code(white, X) :- !,
   sys_white(X).
sys_type_code(space, X) :- !,
   sys_space(X).
sys_type_code(graph, X) :- !,
   sys_graph(X).
sys_type_code(to_lower(Y), X) :- !,
   X = Y.
sys_type_code(to_upper(Y), X) :- !,
   X = Y.
sys_type_code(digit(Y), X) :- !,
   0 =< Y, Y =< 9,
   sys_digit_code(Y, X).
sys_type_code(xdigit(Y), X) :- !,
   0 =< Y, Y =< 15,
   sys_digit_code(Y, X).
sys_type_code(digit, X) :-
   sys_class(X, digit).

% sys_digit_code(+Integer, -Integer)
:- private sys_digit_code/2.
sys_digit_code(Y, X) :- Y < 10, !,
   X is Y+0'0.
sys_digit_code(Y, X) :-
   X is Y-10+0'a.

% sys_white(+-Integer)
:- private sys_white/1.
sys_white(9).
sys_white(32).

% sys_space(+-Integer)
:- private sys_space/1.
sys_space(9).
sys_space(10).
sys_space(11).
sys_space(12).
sys_space(13).
sys_space(X) :- sys_class(X, blank).

% sys_graph(+-Integer)
:- private sys_graph/1.
sys_graph(X) :- var(X), !,
   current_prolog_flag(max_code, M),
   between(0, M, X),
   code_class(X, T),
   T \== blank,
   T \== cntrl,
   T \== inval.
sys_graph(X) :-
   code_class(X, T),
   T \== blank,
   T \== cntrl,
   T \== inval.

% sys_class(+-Integer, -+Atom)
:- private sys_class/2.
sys_class(X, T) :- var(X), !,
   current_prolog_flag(max_code, M),
   between(0, M, X),
   code_class(X, T).
sys_class(X, T) :-
   code_class(X, T).

/**
 * hex_codes(I, L):
 * If L is a list of codes, the predicates succeeds in I
 * with the hex integer represented by the codes L. Otherwise
 * the predicate succeeds in L with the codes of the
 * hex integer I.
 */
% hex_codes(-+Integer, +-List)
:- public hex_codes/2.
hex_codes(I, L) :-
   integer_codes(I, 16, L).
