/**
 * Prolog code for the structure term theory test cases.
 *
 * Source of test cases are the following standards:
 *   - Prolog General Core ISO/IUEC 13211-1
 *   - Draft Technical Corrigendum 2, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2</a>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

/****************************************************************/
/* Building and Unification                                      */
/****************************************************************/

/* X =.. Y */

runner:ref(=.., 2, structure_term, 'ISO 8.5.3.4').
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 1') :-
   foo(a,b) =.. [foo,a,b].
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 2') :-
   X =.. [foo,a,b],
   X == foo(a,b).
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 3') :-
   foo(a,b) =.. L,
   L == [foo,a,b].
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 4') :-
   foo(X,b) =.. [foo,a,Y],
   X == a,
   Y == b.
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 5') :-
   1 =.. [1].
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 6') :-
   \+ foo(a,b) =.. [foo,b,a].
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 7') :-
   catch(_ =.. _, error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 8') :-
   catch(_ =.. [foo,a|_], error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 9') :-
   catch(_ =.. [foo|bar], error(E,_), true),
   nonvar(E),
   E = type_error(list,_).
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 10') :-
   catch(_ =.. [_,bar], error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 11') :-
   catch(_ =.. [3,1], error(E,_), true),
   nonvar(E),
   E = type_error(atom,3).
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 12') :-
   catch(_ =.. [1.1,foo], error(E,_), true),
   nonvar(E),
   E = type_error(atom,1.1).
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 13') :-
   catch(_ =.. [a(b),foo], error(E,_), true),
   nonvar(E),
   E = type_error(atom,a(b)).
runner:case(=.., 2, structure_term, 'ISO 8.5.3.4, ISO 14') :-
   catch(_ =.. 4, error(E,_), true),
   nonvar(E),
   E = type_error(list,4).

/* functor(X, N, A) */

runner:ref(functor, 3, structure_term, 'ISO 8.5.1.4').
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 1') :-
   functor(foo(a,b,c), foo, 3).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 2') :-
   functor(foo(a,b,c), X, Y),
   X == foo,
   Y == 3.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 3') :-
   functor(X, foo, 3),
   nonvar(X),
   X = foo(A,B,C),
   A \== B,
   A \== C,
   B \== C.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 4') :-
   functor(X, foo, 0),
   X == foo.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 5') :-
   functor(mats(A,B), A, B),
   A == mats,
   B == 2.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 6') :-
   \+ functor(foo(a), foo, 2).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 7') :-
   \+ functor(foo(a), fo, 1).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 8') :-
   functor(1, X, Y),
   X == 1,
   Y == 0.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 9') :-
   functor(X, 1.1, 0),
   X == 1.1.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 10') :-
   functor([_|_], '.', 2).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 11') :-
   functor([], [], 0).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 12') :-
   catch(functor(_, _, 3), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 13') :-
   catch(functor(_, foo, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 14') :-
   catch(functor(_, foo, a), error(E,_), true),
   nonvar(E),
   E = type_error(integer,a).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 15') :-
   catch(functor(_, 1.5, 1), error(E,_), true),
   nonvar(E),
   E = type_error(atom,1.5).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 16') :-
   catch(functor(_, foo(a), 1), error(E,_), true),
   nonvar(E),
   E = type_error(_,foo(a)).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 17') :-
   current_prolog_flag(max_arity, A),
   X is A+1,
   catch(functor(_, foo, X), error(E,_), true),
   nonvar(E),
   E = representation_error(_).
runner:case(functor, 3, structure_term, 'ISO 8.5.1.4, ISO 18') :-
   X is -1,
   catch(functor(_, foo, X), error(E,_), true),
   nonvar(E),
   E = domain_error(not_less_than_zero,X).

/* arg(K, X, Y) */

runner:ref(arg, 3, structure_term, 'ISO 8.5.2.4').
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 1') :-
   arg(1, foo(a,b), a).
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 2') :-
   arg(1, foo(a,b), X),
   X == a.
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 3') :-
   arg(1, foo(X,b), a),
   X == a.
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 4') :-
   arg(1, foo(X,b), Y),
   X == Y.
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 5') :-
   \+ arg(1, foo(a,b), b).
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 6') :-
   \+ arg(0, foo(a,b), foo).
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 7') :-
   \+ arg(3, foo(3,4), _).
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 8') :-
   catch(arg(_, foo(a,b), a), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 9') :-
   catch(arg(1, _, a), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 10') :-
   catch(arg(0, atom, _), error(E,_), true),
   nonvar(E),
   E = type_error(compound,atom).
runner:case(arg, 3, structure_term, 'ISO 8.5.2.4, ISO 11') :-
   catch(arg(0, 3, _), error(E,_), true),
   nonvar(E),
   E = type_error(_,3).

/* X = Y */

runner:ref(=, 2, structure_term, 'ISO 8.2.1.4').
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 1') :-
   1 = 1.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 2') :-
   X = 1,
   X == 1.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 3') :-
   X = Y,
   X == Y.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 4') :-
   _ = _.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 5') :-
   X = Y,
   X = abc,
   X == abc,
   Y == abc.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 6') :-
   f(X,def) = f(def,Y),
   X == def,
   Y == def.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 7') :-
   \+ 1 = 2.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 8') :-
   \+ 1 = 1.0.
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 9') :-
   \+ g(X) = f(f(X)).
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 10') :-
   \+ f(X,1) = f(a(X)).
runner:case(=, 2, structure_term, 'ISO 8.2.1.4, ISO 11') :-
   \+ f(X,Y,X) = f(a(X),a(Y),Y,2).

/* unify_with_occurs_check(X, Y) */

runner:ref(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4').
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 1') :-
   unify_with_occurs_check(1, 1).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 2') :-
   unify_with_occurs_check(X, 1),
   X == 1.
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 3') :-
   unify_with_occurs_check(X, Y),
   X == Y.
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 4') :-
   unify_with_occurs_check(_, _).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 5') :-
   unify_with_occurs_check(X, Y),
   unify_with_occurs_check(X, abc),
   X == abc,
   Y == abc.
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 6') :-
   unify_with_occurs_check(f(X,def), f(def,Y)),
   X == def,
   Y == def.
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 7') :-
   \+ unify_with_occurs_check(1, 2).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 8') :-
   \+ unify_with_occurs_check(1, 1.0).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 9') :-
   \+ unify_with_occurs_check(g(X), f(f(X))).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 10') :-
   \+ unify_with_occurs_check(f(X,1), f(a(X))).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 11') :-
   \+ unify_with_occurs_check(f(X,Y,X), f(a(X),a(Y),Y,2)).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 12') :-
   \+ unify_with_occurs_check(X, a(X)).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 13') :-
   \+ unify_with_occurs_check(f(X,1), f(a(X),2)).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 14') :-
   \+ unify_with_occurs_check(f(1,X,1), f(2,a(X),2)).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 15') :-
   \+ unify_with_occurs_check(f(1,X), f(2,a(X))).
runner:case(unify_with_occurs_check, 2, structure_term, 'ISO 8.2.2.4, ISO 16') :-
   \+ unify_with_occurs_check(f(X,Y,X,1), f(a(X),a(Y),Y,2)).

/* X \= Y */

runner:ref(\=, 2, structure_term, 'ISO 8.2.3.4').
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 1') :-
   \+ 1 \= 1.
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 2') :-
   \+ _ \= 1.
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 3') :-
   \+ _ \= _.
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 4') :-
   \+ f(_,def) \= f(def,_).
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 5') :-
   1 \= 2.
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 6') :-
   1 \= 1.0.
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 7') :-
   g(X) \= f(f(X)).
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 8') :-
   f(X,1) \= f(a(X)).
runner:case(\=, 2, structure_term, 'ISO 8.2.3.4, ISO 9') :-
   f(X,Y,X) \= f(a(X),a(Y),Y,2).

/* copy_term(X, Y) */

runner:ref(copy_term, 2, structure_term, 'ISO 8.5.4.4').
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 1') :-
   copy_term(X, Y),
   Y \== X.
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 2') :-
   copy_term(_, 3).
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 3') :-
   copy_term(_, a).
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 4') :-
   copy_term(a+X, X+b),
   X == a.
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 5') :-
   copy_term(_, _).
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 6') :-
   copy_term(X+X+_, A+B+B),
   A == B.
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 7') :-
   \+ copy_term(a, b).
runner:case(copy_term, 2, structure_term, 'ISO 8.5.4.4, ISO 8') :-
   copy_term(a+X, X+b),
   \+ copy_term(a+X, X+b).

/* term_variables(X, L), TC2 8.5.5.4 */

runner:ref(term_variables, 2, structure_term, 'Corr.2 8.5.5.4').
runner:case(term_variables, 2, structure_term, 'Corr.2 8.5.5.4, ISO 1') :-
   term_variables(t, L),
   L == [].
runner:case(term_variables, 2, structure_term, 'Corr.2 8.5.5.4, XLOG 1') :-
   term_variables(a([],X), L),
   L == [X].
runner:case(term_variables, 2, structure_term, 'Corr.2 8.5.5.4, XLOG 2') :-
   C = 3.3*A,
   term_variables(A+B/C, L),
   (  L == [A,B]
   ;  L == [B,A]).

