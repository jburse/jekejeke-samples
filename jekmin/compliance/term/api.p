:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(term/verify)).
:- use_module(library(term/unify)).
:- use_module(library(term/state)).

% put_atts/3
runner:ref(put_atts, 3, term_api, 'Term 0.9.4, 2.1').
runner:case(put_atts, 3, term_api, 'Term 0.9.4, 2.1, XLOG 1') :- true.

% get_atts/3
runner:ref(get_atts, 3, term_api, 'Term 0.9.4, 2.2').
runner:case(get_atts, 3, term_api, 'Term 0.9.4, 2.2, XLOG 1') :- true.

% del_atts/3
runner:ref(del_atts, 2, term_api, 'Term 0.9.4, 2.3').
runner:case(del_atts, 2, term_api, 'Term 0.9.4, 2.3, XLOG 1') :- true.

% put_attr/3
runner:ref(put_attr, 3, term_api, 'Term 0.9.4, 2.4').
runner:case(put_attr, 3, term_api, 'Term 0.9.4, 2.4, XLOG 1') :- true.

% get_attr/3
runner:ref(get_attr, 3, term_api, 'Term 0.9.4, 2.5').
runner:case(get_attr, 3, term_api, 'Term 0.9.4, 2.5, XLOG 1') :- true.

% del_attr/2
runner:ref(del_attr, 2, term_api, 'Term 0.9.4, 2.6').
runner:case(del_attr, 2, term_api, 'Term 0.9.4, 2.6, XLOG 1') :- true.

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
