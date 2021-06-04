/**
 * Prolog code for the api test cases.
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

:- package(library(term)).
:- module(api, []).

:- public runner:ref/4.
:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- public runner:case/4.
:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(system/charsio)).
:- use_module(library(misc/residue)).
:- use_module(library(term/verify)).
:- use_module(library(term/unify)).
:- use_module(library(term/state)).

/*********************************************************************/
/* Verify                                                            */
/*********************************************************************/

:- begin_module(test1).
:- public verify_attributes/2.
verify_attributes(L, _) :- write('L='), write(L), nl.
:- end_module.

% put_atts(+Var, +Term, +Term)
runner:ref(put_atts, 3, term_api, 'Term 1.2.1').
runner:case(put_atts, 3, term_api, 'Term 1.2.1, XLOG 1') :-
   call_residue(put_atts(X, test1, bar), L),
   L == [put_atts(X, test1, bar)].
runner:case(put_atts, 3, term_api, 'Term 1.2.1, XLOG 2') :-
   put_atts(X, test1, bar(Y)), get_atts(X, test1, Z),
   Z == bar(Y).
runner:case(put_atts, 3, term_api, 'Term 1.2.1, XLOG 3') :-
   put_atts(X, test1, baz(_)), put_atts(X, test1, bar(Y)), get_atts(X, test1, Z),
   Z == bar(Y).
runner:case(put_atts, 3, term_api, 'Term 1.2.1, XLOG 4') :-
   catch(put_atts(_, _, bar), error(E, _), true),
   E == instantiation_error.

% get_atts(+Var, -Term, -Term)
runner:ref(get_atts, 3, term_api, 'Term 1.2.2').
runner:case(get_atts, 3, term_api, 'Term 1.2.2, XLOG 1') :-
   \+ get_atts(_, test1, _).
runner:case(get_atts, 3, term_api, 'Term 1.2.2, XLOG 2') :-
   with_output_to(atom(A),
      (put_atts(X, test1, [X, Y]),
      put_atts(Y, test1, [X, Y]), [X, Y] = [1, 2])),
   A == 'L=[_A, _B]\nL=[1, _B]\n'.
runner:case(get_atts, 3, term_api, 'Term 1.2.2, XLOG 3') :-
   with_output_to(atom(A),
      (put_atts(X, test1, [X, Y]),
      put_atts(Y, test1, [X, Y]), X = Y)),
   A == 'L=[_A, _B]\n'.

% del_atts(+Var, +Term)
runner:ref(del_atts, 2, term_api, 'Term 1.2.3').
runner:case(del_atts, 2, term_api, 'Term 1.2.3, XLOG 1') :-
   put_atts(X, test1, bar), del_atts(X, test1), \+ get_atts(X, test1, _).
runner:case(del_atts, 2, term_api, 'Term 1.2.3, XLOG 2') :-
   catch(del_atts(_, f(_)), error(E, _), true),
   E == instantiation_error.

/*********************************************************************/
/* Unify                                                             */
/*********************************************************************/

:- begin_module(test2).
:- public attr_unify_hook/2.
attr_unify_hook(L, _) :- write('L='), write(L), nl.
:- end_module.

% put_attr(+Var, +Term, +Term)
runner:ref(put_attr, 3, term_api, 'Term 1.2.4').
runner:case(put_attr, 3, term_api, 'Term 1.2.4, XLOG 1') :-
   call_residue(put_attr(X, test2, bar), L),
   L == [put_attr(X, test2, bar)].
runner:case(put_attr, 3, term_api, 'Term 1.2.4, XLOG 2') :-
   put_attr(X, test2, bar(Y)), get_attr(X, test2, Z),
   Z == bar(Y).
runner:case(put_attr, 3, term_api, 'Term 1.2.4, XLOG 3') :-
   put_attr(X, test2, baz(_)), put_attr(X, test2, bar(Y)), get_attr(X, test2, Z),
   Z == bar(Y).
runner:case(put_attr, 3, term_api, 'Term 1.2.4, XLOG 4') :-
   catch(put_attr(_, f(_), bar), error(E, _), true),
   E == instantiation_error.

% get_attr(+Var, -Term, -Term)
runner:ref(get_attr, 3, term_api, 'Term 1.2.5').
runner:case(get_attr, 3, term_api, 'Term 1.2.5, XLOG 1') :-
   \+ get_attr(_, test2, _).
runner:case(get_attr, 3, term_api, 'Term 1.2.5, XLOG 2') :-
   with_output_to(atom(A),
      (put_attr(X, test2, [X, Y]),
      put_attr(Y, test2, [X, Y]), [X, Y] = [1, 2])),
   A == 'L=[1, 2]\nL=[1, 2]\n'.
runner:case(get_attr, 3, term_api, 'Term 1.2.5, XLOG 3') :-
   with_output_to(atom(A),
      (put_attr(X, test2, [X, Y]),
      put_attr(Y, test2, [X, Y]), X = Y)),
   A == 'L=[_B, _B]\n'.

% del_attr(+Var, +Term)
runner:ref(del_attr, 2, term_api, 'Term 1.2.6').
runner:case(del_attr, 2, term_api, 'Term 1.2.6, XLOG 1') :-
   put_attr(X, test2, bar), del_attr(X, test2), \+ get_attr(X, test2, _).
runner:case(del_attr, 2, term_api, 'Term 1.2.6, XLOG 2') :-
   catch(del_attr(_, _), error(E, _), true),
   E == instantiation_error.

/*********************************************************************/
/* State                                                             */
/*********************************************************************/

% b_setval(+Term, +Term)
runner:ref(b_setval, 2, term_api, 'Term 1.2.7').
runner:case(b_setval, 2, term_api, 'Term 1.2.7, XLOG 1') :-
   b_setval(foo, bar(Y)), nb_current(foo, X),
   X == bar(Y).
runner:case(b_setval, 2, term_api, 'Term 1.2.7, XLOG 2') :-
   b_setval(foo, baz(_)), b_setval(foo, bar(Z)), nb_current(foo, X),
   X == bar(Z).
runner:case(b_setval, 2, term_api, 'Term 1.2.7, XLOG 3') :-
   b_setval(f(a), g), nb_current(f(a), X),
   X == g.
runner:case(b_setval, 2, term_api, 'Term 1.2.7, XLOG 4') :-
   b_setval(f(a), g), \+ nb_current(f(b), _).
runner:case(b_setval, 2, term_api, 'Term 1.2.7, XLOG 5') :-
   catch(b_setval(_, g), error(E, _), true),
   E == instantiation_error.

% nb_current(-Term, -Term)
runner:ref(nb_current, 2, term_api, 'Term 1.2.8').
runner:case(nb_current, 2, term_api, 'Term 1.2.8, XLOG 1') :-
   \+ nb_current(foo, _).
runner:case(nb_current, 2, term_api, 'Term 1.2.8, XLOG 2') :-
   b_setval(foo, bar), nb_current(foo, X), X == bar.
runner:case(nb_current, 2, term_api, 'Term 1.2.8, XLOG 3') :-
   \+ nb_current(foo, _).

% b_delete(+Term)
runner:ref(b_delete, 1, term_api, 'Term 1.2.9').
runner:case(b_delete, 1, term_api, 'Term 1.2.9, XLOG 1') :-
   b_setval(foo, bar), b_delete(foo), \+ nb_current(foo, _).
runner:case(b_delete, 1, term_api, 'Term 1.2.9, XLOG 2') :-
   b_setval(foo, bar), (b_delete(foo), fail; true),
   nb_current(foo, X), X == bar.
runner:case(b_delete, 1, term_api, 'Term 1.2.9, XLOG 3') :-
   catch(b_delete(f(_)), error(E, _), true),
   E == instantiation_error.
