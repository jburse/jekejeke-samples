:- use_package(library(jekdev/reference/testing)).

:- package(library(term)).
:- module(api, []).

:- public runner:ref/4.
:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- public runner:case/4.
:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(term/verify)).
:- use_module(library(term/unify)).
:- use_module(library(term/state)).

/*********************************************************************/
/* Verify                                                            */
/*********************************************************************/

:- begin_module(test1).
verify_attributes(_, _, true).
:- end_module.

% put_atts(+Var, +Term, +Term)
runner:ref(put_atts, 3, term_api, 'Term 1.0.0, 2.1').
runner:case(put_atts, 3, term_api, 'Term 1.0.0, 2.1, XLOG 1') :-
   put_atts(X, test1, bar(Y)),
   get_atts(X, test1, Z),
   Z == bar(Y).
runner:case(put_atts, 3, term_api, 'Term 1.0.0, 2.1, XLOG 2') :-
   put_atts(X, test1, baz(_)),
   put_atts(X, test1, bar(Y)),
   get_atts(X, test1, Z),
   Z == bar(Y).
runner:case(put_atts, 3, term_api, 'Term 1.0.0, 2.1, XLOG 3') :-
   catch(put_atts(_, _, bar), error(E,_), true),
   E == instantiation_error.

% get_atts(+Var, -Term, -Term)
runner:ref(get_atts, 3, term_api, 'Term 1.0.0, 2.2').
runner:case(get_atts, 3, term_api, 'Term 1.0.0, 2.2, XLOG 1') :-
   \+ get_atts(_, test1, _).

% del_atts(+Var, +Term)
runner:ref(del_atts, 2, term_api, 'Term 1.0.0, 2.3').
runner:case(del_atts, 2, term_api, 'Term 1.0.0, 2.3, XLOG 1') :-
   put_atts(X, test1, bar),
   del_atts(X, test1),
   \+ get_atts(X, test1, _).
runner:case(del_atts, 2, term_api, 'Term 1.0.0, 2.3, XLOG 2') :-
   catch(del_atts(_, f(_)), error(E,_), true),
   E == instantiation_error.

/*********************************************************************/
/* Unify                                                             */
/*********************************************************************/

:- begin_module(test2).
attr_unify_hook(_, _).
:- end_module.

% put_attr(+Var, +Term, +Term)
runner:ref(put_attr, 3, term_api, 'Term 1.0.0, 2.4').
runner:case(put_attr, 3, term_api, 'Term 1.0.0, 2.4, XLOG 1') :-
   put_attr(X, test2, bar(Y)),
   get_attr(X, test2, Z),
   Z == bar(Y).
runner:case(put_attr, 3, term_api, 'Term 1.0.0, 2.4, XLOG 2') :-
   put_attr(X, test2, baz(_)),
   put_attr(X, test2, bar(Y)),
   get_attr(X, test2, Z),
   Z == bar(Y).
runner:case(put_attr, 3, term_api, 'Term 1.0.0, 2.4, XLOG 3') :-
   catch(put_attr(_, f(_), bar), error(E,_), true),
   E == instantiation_error.

% get_attr(+Var, -Term, -Term)
runner:ref(get_attr, 3, term_api, 'Term 1.0.0, 2.5').
runner:case(get_attr, 3, term_api, 'Term 1.0.0, 2.5, XLOG 1') :-
   \+ get_attr(_, test2, _).

% del_attr(+Var, +Term)
runner:ref(del_attr, 2, term_api, 'Term 1.0.0, 2.6').
runner:case(del_attr, 2, term_api, 'Term 1.0.0, 2.6, XLOG 1') :-
   put_attr(X, test2, bar),
   del_attr(X, test2),
   \+ get_attr(X, test2, _).
runner:case(del_attr, 2, term_api, 'Term 1.0.0, 2.6, XLOG 2') :-
   catch(del_attr(_, _), error(E,_), true),
   E == instantiation_error.

/*********************************************************************/
/* State                                                             */
/*********************************************************************/

% b_setval(+Term, +Term)
runner:ref(b_setval, 2, term_api, 'Term 1.0.0, 2.7').
runner:case(b_setval, 2, term_api, 'Term 1.0.0, 2.7, XLOG 1') :-
   b_setval(foo, bar(Y)),
   nb_current(foo, X),
   X == bar(Y).
runner:case(b_setval, 2, term_api, 'Term 1.0.0, 2.7, XLOG 2') :-
   b_setval(foo, baz(_)),
   b_setval(foo, bar(Z)),
   nb_current(foo, X),
   X == bar(Z).
runner:case(b_setval, 2, term_api, 'Term 1.0.0, 2.7, XLOG 3') :-
   b_setval(f(a), g),
   nb_current(f(a), X),
   X == g.
runner:case(b_setval, 2, term_api, 'Term 1.0.0, 2.7, XLOG 4') :-
   b_setval(f(a), g),
   \+ nb_current(f(b), _).
runner:case(b_setval, 2, term_api, 'Term 1.0.0, 2.7, XLOG 5') :-
   catch(b_setval(_, g), error(E,_), true),
   E == instantiation_error.

% nb_current(-Term, -Term)
runner:ref(nb_current, 2, term_api, 'Term 1.0.0, 2.8').
runner:case(nb_current, 2, term_api, 'Term 1.0.0, 2.8, XLOG 1') :-
   \+ nb_current(foo, _).
runner:case(nb_current, 2, term_api, 'Term 1.0.0, 2.8, XLOG 2') :-
   b_setval(foo, bar),
   nb_current(foo, X),
   X == bar.
runner:case(nb_current, 2, term_api, 'Term 1.0.0, 2.8, XLOG 3') :-
   \+ nb_current(foo, _).

% b_delete(+Term)
runner:ref(b_delete, 1, term_api, 'Term 1.0.0, 2.9').
runner:case(b_delete, 1, term_api, 'Term 1.0.0, 2.9, XLOG 1') :-
   b_setval(foo, bar),
   b_delete(foo),
   \+ nb_current(foo, _).
runner:case(b_delete, 1, term_api, 'Term 1.0.0, 2.9, XLOG 2') :-
   b_setval(foo, bar),
   (  b_delete(foo), fail; true),
   nb_current(foo, X),
   X == bar.
runner:case(b_delete, 1, term_api, 'Term 1.0.0, 2.9, XLOG 3') :-
   catch(b_delete(f(_)), error(E,_), true),
   E == instantiation_error.
