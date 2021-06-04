/**
 * Prolog code for the delay test cases.
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

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(misc/residue)).
:- use_module(library(term/herbrand)).
:- use_module(library(term/suspend)).

/* sto(+Term) */

runner:ref(sto, 1, term_delay, 'Term 1.1.1').
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 1') :-
   call_residue(sto(X), L), L == [sto(X)].
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 2') :-
   call_residue((sto(X), X = f(Y, Z, T)), L), L == [sto(Y), sto(Z), sto(T)].
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 3') :-
   \+ (sto(X), X = f(X)).
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 4') :-
   \+ (sto(X), X = g(Y), Y = f(Y)).
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 5') :-
   call_residue((sto(_), fail; true), L), L == [].
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 6a') :-
   findall(L-X-Y, call_residue((sto(X); sto(Y)), L), R),
   R = [[sto(A)]-B-_|_], A == B.
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 6b') :-
   findall(L-X-Y, call_residue((sto(X); sto(Y)), L), R),
   R = [_, [sto(A)]-_-B|_], A == B.
runner:case(sto, 1, term_delay, 'Term 1.1.1, XLOG 6c') :-
   findall(L-X-Y, call_residue((sto(X); sto(Y)), L), R),
   R = [_, _].

/* dif(+Term, +Term) */

runner:ref(dif, 2, term_delay, 'Term 1.1.2').
runner:case(dif, 2, term_delay, 'Term 1.1.2, XLOG 1') :-
   call_residue(dif(f(X, X), f(Y, Z)), L),
   L == [dif((Y, X), (Z, Y))].
runner:case(dif, 2, term_delay, 'Term 1.1.2, XLOG 2') :-
   dif(f, f(_, _)).
runner:case(dif, 2, term_delay, 'Term 1.1.2, XLOG 3') :-
   dif(f(X, Y), g(X, Y)).
runner:case(dif, 2, term_delay, 'Term 1.1.2, XLOG 4') :-
   \+ dif(f(X, Y), f(X, Y)).
runner:case(dif, 2, term_delay, 'Term 1.1.2, SWI7 2') :-
   dif(1, X), \+ X = 1.
runner:case(dif, 2, term_delay, 'Term 1.1.2, SWI7 3') :-
   dif(1, X), dif(X, 2), \+ X = 1, \+ X = 2.
runner:case(dif, 2, term_delay, 'Term 1.1.2, SWI7 4') :-
   dif(X, Y), X = 1, \+ Y = 1.
runner:case(dif, 2, term_delay, 'Term 1.1.2, SWI7 5') :-
   dif(X, Y), \+ X = Y.
runner:case(dif, 2, term_delay, 'Term 1.1.2, SWI7 6') :-
   dif(X-Z, a-b), dif(X-_, b-b),
   X = a, \+ Z = b.
runner:case(dif, 2, term_delay, 'Term 1.1.2, SWI7 7') :-
   dif(X-Z, a-b), dif(X-Y, b-b),
   Y = b, Z = b, \+ X = a, \+ X = b.
runner:case(dif, 2, term_delay, 'Term 1.0.8, 1.2, XLOG 5') :-
   call_residue((dif((Y, Z), (Z, T)), T = Z), L),
   L == [dif(Y, Z)].
runner:case(dif, 2, term_delay, 'Term 1.0.8, 1.2, XLOG 6') :-
   call_residue((dif((Y, Z), (Z, T)), Z = T), L),
   L == [dif(Y, T)].
runner:case(dif, 2, term_delay, 'Term 1.1.6, 1.2, XLOG 7') :-
   call_residue((dif(X, f(Y)), Y = X), L),
   L == [dif(Y, f(Y))].

/* unifiable(+Term, +Term, -List) */

runner:ref(unifiable, 3, term_delay, 'Term 1.1.6, 1.3').
runner:case(unifiable, 3, term_delay, 'Term 1.1.6, 1.3, XLOG 1') :-
   unifiable(f(X, X), f(Y, Z), L),
   L == [Y-Z, X-Y].
runner:case(unifiable, 3, term_delay, 'Term 1.1.6, 1.3, XLOG 2') :-
   \+ unifiable(f, f(_, _), _).
runner:case(unifiable, 3, term_delay, 'Term 1.1.6, 1.3, XLOG 3') :-
   \+ unifiable(f(X, Y), g(X, Y), _).
runner:case(unifiable, 3, term_delay, 'Term 1.1.6, 1.3, XLOG 4') :-
   unifiable(f(X, Y), f(X, Y), L),
   L == [].
runner:case(unifiable, 3, term_delay, 'Term 1.1.6, 1.3, XLOG 5') :-
   unifiable(X, f(X), L),
   L == [X-f(X)].

/* dif_with_occurs_check(+Term, +Term) */

runner:ref(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4').
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 1') :-
   call_residue(dif_with_occurs_check(f(X, X), f(Y, Z)), L),
   L == [dif_with_occurs_check((Y, X), (Z, Y))].
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 2') :-
   dif_with_occurs_check(f, f(_, _)).
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 3') :-
   dif_with_occurs_check(f(X, Y), g(X, Y)).
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 4') :-
   \+ dif_with_occurs_check(f(X, Y), f(X, Y)).
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, SWI7 2') :-
   dif_with_occurs_check(1, X), \+ X = 1.
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, SWI7 3') :-
   dif_with_occurs_check(1, X), dif_with_occurs_check(X, 2), \+ X = 1, \+ X = 2.
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, SWI7 4') :-
   dif_with_occurs_check(X, Y), X = 1, \+ Y = 1.
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, SWI7 5') :-
   dif_with_occurs_check(X, Y), \+ X = Y.
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, SWI7 6') :-
   dif_with_occurs_check(X-Z, a-b), dif_with_occurs_check(X-_, b-b),
   X = a, \+ Z = b.
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, SWI7 7') :-
   dif_with_occurs_check(X-Z, a-b), dif_with_occurs_check(X-Y, b-b),
   Y = b, Z = b, \+ X = a, \+ X = b.
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 5') :-
   call_residue((dif_with_occurs_check((Y, Z), (Z, T)), T = Z), L),
   L == [dif_with_occurs_check(Y, Z)].
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 6') :-
   call_residue((dif_with_occurs_check((Y, Z), (Z, T)), Z = T), L),
   L == [dif_with_occurs_check(Y, T)].
runner:case(dif_with_occurs_check, 2, term_delay, 'Term 1.1.6, 1.4, XLOG 7') :-
   call_residue((dif_with_occurs_check(X, f(Y)), Y = X), L),
   L == [].

/* unifiable_with_occurs_check(+Term, +Term, -List) */

runner:ref(unifiable_with_occurs_check, 3, term_delay, 'Term 1.1.6, 1.5').
runner:case(unifiable_with_occurs_check, 3, term_delay, 'Term 1.1.6, 1.5, XLOG 1') :-
   unifiable_with_occurs_check(f(X, X), f(Y, Z), L),
   L == [Y-Z, X-Y].
runner:case(unifiable_with_occurs_check, 3, term_delay, 'Term 1.1.6, 1.5, XLOG 2') :-
   \+ unifiable_with_occurs_check(f, f(_, _), _).
runner:case(unifiable_with_occurs_check, 3, term_delay, 'Term 1.1.6, 1.5, XLOG 3') :-
   \+ unifiable_with_occurs_check(f(X, Y), g(X, Y), _).
runner:case(unifiable_with_occurs_check, 3, term_delay, 'Term 1.1.6, 1.5, XLOG 4') :-
   unifiable_with_occurs_check(f(X, Y), f(X, Y), L),
   L == [].
runner:case(unifiable_with_occurs_check, 3, term_delay, 'Term 1.1.6, 1.5, XLOG 5') :-
   \+ unifiable_with_occurs_check(X, f(X), _).

/********************************************************************/
/* Freeze & When                                                    */
/********************************************************************/

/* freeze(+Term, +Goal) */

runner:ref(freeze, 2, term_delay, 'Term 1.1.6').
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 1') :-
   call_residue((freeze(X, X > 0), freeze(X, X < 0)), L),
   L == [freeze(X, X > 0), freeze(X, X < 0)].
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 2') :-
   freeze(X, X > 0), freeze(X, X < 0), \+ X = 1, \+ X = -1.
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 3') :-
   freeze(X, X > Y), Y = 0, \+ X = -1.
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 4') :-
   freeze(X, (X = f(a); X = f(b))), \+ X = g(_).
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 5') :-
   findall(X, (freeze(X, (X = f(a); X = f(b))), X = f(_)), [Y|_]),
   Y == f(a).
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 6') :-
   findall(X, (freeze(X, (X = f(a); X = f(b))), X = f(_)), [_, Y|_]),
   Y == f(b).
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 7') :-
   findall(X, (freeze(X, (X = f(a); X = f(b))), X = f(_)), [_, _]).
runner:case(freeze, 2, term_delay, 'Term 1.1.6, XLOG 8') :-
   freeze(X, 1), catch(X = 0, error(E, _), true),
   E = type_error(callable, 1).

/* when(+Cond, +Goal) */

runner:ref(when, 2, term_delay, 'Term 1.1.7').
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 1') :-
   call_residue(when((nonvar(X), nonvar(Y)), X < Y), L),
   L == [when((nonvar(X), nonvar(Y)), X < Y)].
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 2') :-
   call_residue((when((nonvar(X), nonvar(Y)), X < Y), X = 0), L),
   L == [when(nonvar(Y), 0 < Y)].
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 3') :-
   when((nonvar(X), nonvar(Y)), X < Y), \+ (X = 0, Y = -1), \+ (X = 1, Y = 0).
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 4') :-
   when((nonvar(X); ground(Y)), atom_codes(X, Y)), X = abc,
   Y == "abc".
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 5') :-
   when((nonvar(X); ground(Y)), atom_codes(X, Y)), Y = "abc",
   X == abc.
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 6') :-
   findall(X, (when(nonvar(X), (X = f(a); X = f(b))), X = f(_)), [Y|_]),
   Y == f(a).
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 7') :-
   findall(X, (when(nonvar(X), (X = f(a); X = f(b))), X = f(_)), [_, Y|_]),
   Y == f(b).
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 8') :-
   findall(X, (when(nonvar(X), (X = f(a); X = f(b))), X = f(_)), [_, _]).
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 9') :-
   when(nonvar(X), _), catch(X = 0, error(E, _), true),
   E == instantiation_error.
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 10') :-
   catch(when(_, true), error(E, _), true),
   E == instantiation_error.
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 11') :-
   call_residue((when((nonvar(X), nonvar(Y)), X < Y), Y = 1), L),
   L == [when((nonvar(X), nonvar(1)), X < 1)].
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 12') :-
   call_residue((when((nonvar(X); ground(Y)), atom_codes(X, Y)), Y = [65|R]), L),
   L == [when((nonvar(X); ground([R])), atom_codes(X, [65|R]))].
runner:case(when, 2, term_delay, 'Term 1.1.7, XLOG 13') :-
   call_residue((when((nonvar(X); ground(Y)), atom_codes(X, Y)), Y = [C, 65|R]), L),
   L == [when((nonvar(X); ground([C, R])), atom_codes(X, [C, 65|R]))].
