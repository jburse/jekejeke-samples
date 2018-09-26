/**
 * Prolog code for the structure type theory test cases.
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

/****************************************************************/
/* Type Testing                                                 */
/****************************************************************/

/* var(X) */
runner:ref(var, 1, structure_intatom, 'ISO 8.3.1.4').
runner:case(var, 1, structure_intatom, 'ISO 8.3.1.4, ISO 1') :-
   \+ var(foo).
runner:case(var, 1, structure_intatom, 'ISO 8.3.1.4, ISO 3') :-
   \+ (  foo = Foo,
         var(Foo)).
runner:case(var, 1, structure_intatom, 'ISO 8.3.1.4, ISO 4') :-
   var(_).

/* nonvar(X) */
runner:ref(nonvar, 1, structure_intatom, 'ISO 8.3.7.4').
% runner:case(nonvar, 1, structure_intatom, _) :- nonvar(3).
runner:case(nonvar, 1, structure_intatom, 'ISO 8.3.7.4, ISO 1') :-
   nonvar(33.3).
runner:case(nonvar, 1, structure_intatom, 'ISO 8.3.7.4, ISO 2') :-
   nonvar(foo).
runner:case(nonvar, 1, structure_intatom, 'ISO 8.3.7.4, ISO 4') :-
   foo = Foo,
   nonvar(Foo).
runner:case(nonvar, 1, structure_intatom, 'ISO 8.3.7.4, ISO 5') :-
   \+ nonvar(_).
runner:case(nonvar, 1, structure_intatom, 'ISO 8.3.7.4, ISO 6') :-
   nonvar(a(b)).

/* atom(X) */
runner:ref(atom, 1, structure_intatom, 'ISO 8.3.2.4').
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 1') :-
   atom(atom).
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 2') :-
   atom(string).
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 3') :-
   \+ atom(a(b)).
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 4') :-
   \+ atom(_).
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 5') :-
   atom([]).
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 6') :-
   \+ atom(6).
runner:case(atom, 1, structure_intatom, 'ISO 8.3.2.4, ISO 7') :-
   \+ atom(3.3).

/* integer(X) */
runner:ref(integer, 1, structure_intatom, 'ISO 8.3.3.4').
runner:case(integer, 1, structure_intatom, 'ISO 8.3.3.4, ISO 1') :-
   integer(3).
runner:case(integer, 1, structure_intatom, 'ISO 8.3.3.4, ISO 2') :-
   integer(-3).
runner:case(integer, 1, structure_intatom, 'ISO 8.3.3.4, ISO 3') :-
   \+ integer(3.3).
runner:case(integer, 1, structure_intatom, 'ISO 8.3.3.4, ISO 4') :-
   \+ integer(_).
runner:case(integer, 1, structure_intatom, 'ISO 8.3.3.4, ISO 5') :-
   \+ integer(atom).

/* atomic(X) */
runner:ref(atomic, 1, structure_intatom, 'ISO 8.3.5.4').
runner:case(atomic, 1, structure_intatom, 'ISO 8.3.5.4, ISO 1') :-
   atomic(atom).
runner:case(atomic, 1, structure_intatom, 'ISO 8.3.5.4, ISO 2') :-
   \+ atomic(a(b)).
runner:case(atomic, 1, structure_intatom, 'ISO 8.3.5.4, ISO 3') :-
   \+ atomic(_).
runner:case(atomic, 1, structure_intatom, 'ISO 8.3.5.4, ISO 4') :-
   atomic(6).
runner:case(atomic, 1, structure_intatom, 'ISO 8.3.5.4, ISO 5') :-
   atomic(3.3).

/* float(X) */
runner:ref(float, 1, structure_intatom, 'ISO 8.3.4.4').
runner:case(float, 1, structure_intatom, 'ISO 8.3.4.4, ISO 1') :-
   float(3.3).
runner:case(float, 1, structure_intatom, 'ISO 8.3.4.4, ISO 2') :-
   float(-3.3).
runner:case(float, 1, structure_intatom, 'ISO 8.3.4.4, ISO 3') :-
   \+ float(3).
runner:case(float, 1, structure_intatom, 'ISO 8.3.4.4, ISO 4') :-
   \+ float(atom).
runner:case(float, 1, structure_intatom, 'ISO 8.3.4.4, ISO 5') :-
   \+ float(_).

/* compound(X) */
runner:ref(compound, 1, structure_intatom, 'ISO 8.3.6.4').
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 1') :-
   \+ compound(33.3).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 2') :-
   \+ compound(-33.3).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 3') :-
   compound(-a).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 4') :-
   \+ compound(_).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 5') :-
   \+ compound(a).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 6') :-
   compound(a(b)).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 7') :-
   \+ compound([]).
runner:case(compound, 1, structure_intatom, 'ISO 8.3.6.4, ISO 8') :-
   compound([b]).

/* number(X) */
runner:ref(number, 1, structure_intatom, 'ISO 8.3.8.4').
runner:case(number, 1, structure_intatom, 'ISO 8.3.8.4, ISO 1') :-
   number(3).
runner:case(number, 1, structure_intatom, 'ISO 8.3.8.4, ISO 2') :-
   number(3.3).
runner:case(number, 1, structure_intatom, 'ISO 8.3.8.4, ISO 3') :-
   number(-3).
runner:case(number, 1, structure_intatom, 'ISO 8.3.8.4, ISO 4') :-
   \+ number(a).
runner:case(number, 1, structure_intatom, 'ISO 8.3.8.4, ISO 5') :-
   \+ number(_).

/* callable(X) */
runner:ref(callable, 1, structure_intatom, 'Corr.2 8.3.9.4').
runner:case(callable, 1, structure_intatom, 'Corr.2 8.3.9.4, ISO 1') :-
   callable(a).
runner:case(callable, 1, structure_intatom, 'Corr.2 8.3.9.4, ISO 2') :-
   \+ callable(3).
runner:case(callable, 1, structure_intatom, 'Corr.2 8.3.9.4, ISO 3') :-
   \+ callable(_).
runner:case(callable, 1, structure_intatom, 'Corr.2 8.3.9.4, ISO 4') :-
   callable((1,2)).
runner:case(callable, 1, structure_intatom, 'Corr.2 8.3.9.4, XLOG 1') :-
   callable(string).

/* ground(X) */
runner:ref(ground, 1, structure_intatom, 'Corr.2 8.3.10.4').
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, ISO 1') :-
   ground(3).
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, ISO 2') :-
   \+ ground(a(1,_)).
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, XLOG 1') :-
   ground(atom).
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, XLOG 2') :-
   ground(a(b)).
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, XLOG 3') :-
   foo = Foo,
   ground(Foo).
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, XLOG 4') :-
   \+ ground(_).
runner:case(ground, 1, structure_intatom, 'Corr.2 8.3.10.4, XLOG 5') :-
   ground([]).

/* acyclic_term(X) */
runner:ref(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4').
runner:case(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4, ISO 1') :-
   acyclic_term(a(_,1)).
runner:case(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4, ISO 2') :-
   X = f(X),
   \+ acyclic_term(X).
runner:case(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4, XLOG 1') :-
   X = f(X),
   Y = g(X),
   \+ acyclic_term(Y).
runner:case(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4, XLOG 2') :-
   X = f(Y),
   Y = g(X),
   Z = h(X),
   \+ acyclic_term(Z).
runner:case(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4, XLOG 3') :-
   X = [a,b,c|Y],
   Y = [d,e,f|Z],
   Z = [g,h,i],
   acyclic_term(X).
runner:case(acyclic_term, 1, structure_intatom, 'Corr.2 8.3.11.4, XLOG 4') :-
   X = [a,b,c|Y],
   Y = [d,e,f|Z],
   Z = [g,h,i|X],
   \+ acyclic_term(X).

