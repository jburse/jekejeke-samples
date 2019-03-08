/**
 * Prolog code for the dot notation on tagged structures test cases.
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

:- use_module(library(notebook/dict)).
:- use_module(library(notebook/func)).

/**********************************************************/
/* Tagged Structures                                      */
/**********************************************************/

/* D.F := X */

min_coord(P, P.x) :-
   P.x < P.y, !.
min_coord(P, P.y).

runner:ref(dict_clause, 2, obsolete_taggeddot, 'SWI7 1.1').
runner:case(dict_clause, 2, obsolete_taggeddot, 'SWI7 1.1, XLOG 1') :-
   min_coord(point{x:1,y:2}, M),
   M == 1.
runner:case(dict_clause, 2, obsolete_taggeddot, 'SWI7 1.1, XLOG 2') :-
   min_coord(point{x:3,y:2}, M),
   M == 2.
runner:case(dict_clause, 2, obsolete_taggeddot, 'SWI7 1.1, XLOG 3') :-
   \+ min_coord(point{x:1,y:2}, 2).
runner:case(dict_clause, 2, obsolete_taggeddot, 'SWI7 1.1, XLOG 4') :-
   \+ min_coord(point{x:3,y:2}, 3).

/* D.F */

runner:ref(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2').
runner:case(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2, XLOG 1') :-
   P = point{x:1,y:2},
   X = P.x,
   Y = P.y,
   X == 1,
   Y == 2.
runner:case(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2, XLOG 2') :-
   P = point{x:1,y:2},
   findall(K-V, V = P.K, [J-W|_]),
   J == x,
   W == 1.
runner:case(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2, XLOG 3') :-
   P = point{x:1,y:2},
   findall(K-V, V = P.K, [_,J-W|_]),
   J == y,
   W == 2.
runner:case(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2, XLOG 4') :-
   P = point{x:1,y:2},
   findall(K-V, V = P.K, [_,_]).
runner:case(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2, XLOG 5') :-
   B = book{author:person{name:bob}},
   N = B.author.name,
   N == bob.
runner:case(dict_access, 2, obsolete_taggeddot, 'SWI7 1.2, XLOG 6') :-
   P = point{x:1,y:2},
   catch(_ = P.z, error(E,_), true),
   E = existence_error(key,z).


/* D.get(K) */

runner:ref(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3').
runner:case(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3, XLOG 1') :-
   P = point{x:1,y:2},
   X = P.get(x),
   Y = P.get(y),
   X == 1,
   Y == 2.
runner:case(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3, XLOG 2') :-
   P = point{x:1,y:2},
   findall(K-V, V = P.get(K), [J-W|_]),
   J == x,
   W == 1.
runner:case(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3, XLOG 3') :-
   P = point{x:1,y:2},
   findall(K-V, V = P.get(K), [_,J-W|_]),
   J == y,
   W == 2.
runner:case(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3, XLOG 4') :-
   P = point{x:1,y:2},
   findall(K-V, V = P.get(K), [_,_]).
runner:case(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3, XLOG 5') :-
   B = book{author:person{name:bob}},
   N = B.get(author).get(name),
   N == bob.
runner:case(dict_get, 2, obsolete_taggeddot, 'SWI7 1.3, XLOG 6') :-
   P = point{x:1,y:2},
   \+ _ = P.get(z).

/* D.put(E) */

runner:ref(dict_put, 2, obsolete_taggeddot, 'SWI7 1.4').
runner:case(dict_put, 2, obsolete_taggeddot, 'SWI7 1.4, XLOG 1') :-
   P = point{x:1,y:2},
   Q = P.put(_{x:X}),
   Q == point{x:X,y:2}.
runner:case(dict_put, 2, obsolete_taggeddot, 'SWI7 1.4, XLOG 2') :-
   P = point{x:1,y:2},
   Q = P.put(_{z:3}),
   Q == point{x:1,y:2,z:3}.
runner:case(dict_put, 2, obsolete_taggeddot, 'SWI7 1.4, XLOG 3') :-
   P = point{x:1,y:2},
   Q = P.put(_{z:Z,x:4}),
   Q == point{x:4,y:2,z:Z}.
runner:case(dict_put, 2, obsolete_taggeddot, 'SWI7 1.4, XLOG 4') :-
   P = point{x:1,y:2},
   catch(_ = P.put(123), error(E,_), true),
   E == type_error(dict,123).

/* D.put(K,V) */

runner:ref(dict_put, 3, obsolete_taggeddot, 'SWI7 1.5').
runner:case(dict_put, 3, obsolete_taggeddot, 'SWI7 1.5, XLOG 1') :-
   P = point{x:1,y:2},
   Q = P.put(x,X),
   Q == point{x:X,y:2}.
runner:case(dict_put, 3, obsolete_taggeddot, 'SWI7 1.5, XLOG 2') :-
   P = point{x:1,y:2},
   Q = P.put(z,3),
   Q == point{x:1,y:2,z:3}.
runner:case(dict_put, 3, obsolete_taggeddot, 'SWI7 1.5, XLOG 3') :-
   P = point{x:1,y:2},
   Q = P.put(z,Z).put(x,4),
   Q == point{x:4,y:2,z:Z}.
runner:case(dict_put, 3, obsolete_taggeddot, 'SWI7 1.5, XLOG 4') :-
   P = point{x:1,y:2},
   catch(_ = P.put(_,3), error(E,_), true),
   E == instantiation_error.

/* D.F() and D.F(X1,..,Xn) */

Pt.offset(Dx,Dy) := point{x:X,y:Y} :-
   X is Pt.x+Dx,
   Y is Pt.y+Dy.
Pt.dist() := D :-
   D is sqrt(Pt.x**2+Pt.y**2).

runner:ref(dict_call, 2, obsolete_taggeddot, 'SWI7 1.6').
runner:case(dict_call, 2, obsolete_taggeddot, 'SWI7 1.6, XLOG 1') :-
   P = point{x:1,y:2},
   Q = P.offset(3,4),
   Q == point{x:4,y:6}.
runner:case(dict_call, 2, obsolete_taggeddot, 'SWI7 1.6, XLOG 2') :-
   P = point{x:1,y:2},
   catch(_ = P.foo(77), error(E,_), true),
   nonvar(E),
   E = existence_error(procedure,_).
runner:case(dict_call, 2, obsolete_taggeddot, 'SWI7 1.6, XLOG 3') :-
   D = point{x:1,y:2}.dist(),
   D == 2.23606797749979.
runner:case(dict_call, 2, obsolete_taggeddot, 'SWI7 1.6, XLOG 4') :-
   D = point{x:1,y:2}.offset(3,4).dist(),
   D == 7.211102550927978.


