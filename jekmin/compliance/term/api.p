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

% put_attr/3
runner:ref(put_attr, 3, term_api, 'Term 0.9.4, 2.2').
runner:case(put_attr, 3, term_api, 'Term 0.9.4, 2.2, XLOG 1') :- true.

% b_setval/2
runner:ref(b_setval, 2, term_api, 'Term 0.9.4, 2.3').
runner:case(b_setval, 2, term_api, 'Term 0.9.4, 2.3, XLOG 1') :- true.
