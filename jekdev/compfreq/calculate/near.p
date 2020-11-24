/**
 * Prolog code for the calculate near test cases.
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

:- use_module(library(standard/approx)).
:- use_module(library(basic/lists)).
:- use_module(library(basic/random)).
:- use_module(library(advanced/sets)).

/****************************************************************/
/* approx.p extras                                              */
/****************************************************************/

/* rational(X) */

runner:ref(rational, -2, calculate_near, 'XLOG 3.1.1').
runner:case(rational, -2, calculate_near, 'XLOG 3.1.1, XLOG 1') :-
   X is rational(-89/21), X == -4771671033761597#1125899906842624.
runner:case(rational, -2, calculate_near, 'XLOG 3.1.1, XLOG 2') :-
   X is rational(8*(pi-3)), X == 39854788871587#35184372088832.
runner:case(rational, -2, calculate_near, 'XLOG 3.1.1, XLOG 3') :-
   X is rational(903/(pi+903)), X == 35062387880435#35184372088832.

/* rationalize(X) */

runner:ref(rationalize, -2, calculate_near, 'XLOG 3.1.2').
runner:case(rationalize, -2, calculate_near, 'XLOG 3.1.2, XLOG 1') :-
   X is rationalize(-89/21), X == -89#21.
runner:case(rationalize, -2, calculate_near, 'XLOG 3.1.2, XLOG 2') :-
   X is rationalize(8*(pi-3)), X == 121642183#107387442.
runner:case(rationalize, -2, calculate_near, 'XLOG 3.1.2, XLOG 3') :-
   X is rationalize(903/(pi+903)), X == 222529257#223303450.

/* rationalize32(X) */

runner:ref(rationalize32, -2, calculate_near, 'XLOG 3.1.3').
runner:case(rationalize32, -2, calculate_near, 'XLOG 3.1.3, XLOG 1') :-
   X is rationalize32(-89/21), X == -89#21.
runner:case(rationalize32, -2, calculate_near, 'XLOG 3.1.3, XLOG 2') :-
   X is rationalize32(8*(pi-3)), X == 4591#4053.
runner:case(rationalize32, -2, calculate_near, 'XLOG 3.1.3, XLOG 3') :-
   X is rationalize32(903/(pi+903)), X == 10635#10672.

/* number_compare(C, X, Y) */

runner:ref(number_compare, 3, calculate_near, 'XLOG 3.1.4').
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 1') :-
   number_compare(C, 5#2, 10#7), C == > .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 2') :-
   number_compare(C, -5#2, 3), C == < .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 3') :-
   number_compare(C, t(4), t(10#7)), C == > .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 4') :-
   number_compare(C, 12, -6), C == > .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 5') :-
   number_compare(C, t(2#3), t(2#3)), C == = .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 6') :-
   number_compare(C, -10, -10), C == = .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 7') :-
   number_compare(C, foo(a, b), north(a)), C == > .
runner:case(number_compare, 3, calculate_near, 'XLOG 3.1.4, XLOG 6') :-
   number_compare(C, foo(a, _), foo(b, _)), C == < .

/****************************************************************/
/* lists.p extras                                               */
/****************************************************************/

runner:ref(length, 2, calculate_near, 'XLOG 3.2.1').
runner:case(length, 2, calculate_near, 'XLOG 3.2.1, XLOG 1') :-
   length([a, b, c], N),
   N == 3.
runner:case(length, 2, calculate_near, 'XLOG 3.2.1, XLOG 2') :-
   length(L, 3), length(L, N),
   N == 3.

runner:ref(nth0, 3, calculate_near, 'XLOG 3.2.2').
runner:case(nth0, 3, calculate_near, 'XLOG 3.2.2, XLOG 1') :-
   nth0(1, [a, b, c], X),
   X == b.
runner:case(nth0, 3, calculate_near, 'XLOG 3.2.2, XLOG 2') :-
   nth0(K, [a, b, c], c),
   K == 2.

runner:ref(nth0, 4, calculate_near, 'XLOG 3.2.3').
runner:case(nth0, 4, calculate_near, 'XLOG 3.2.3, XLOG 1') :-
   nth0(0, [a, b, c], X, R),
   X == a, R == [b, c].
runner:case(nth0, 4, calculate_near, 'XLOG 3.2.3, XLOG 2') :-
   nth0(J, [a, b, c], b, R),
   J == 1, R == [a, c].

/****************************************************************/
/* random.p extras                                              */
/****************************************************************/

runner:ref(random_permutation, 2, calculate_near, 'XLOG 3.3.1').
runner:case(random_permutation, 2, calculate_near, 'XLOG 3.3.1, XLOG 1') :-
   random_permutation([a, b, c], X),
   equal(X, [a, b, c]).
runner:case(random_permutation, 2, calculate_near, 'XLOG 3.3.1, XLOG 2') :-
   catch(random_permutation([a|_], _), error(E, _), true),
   E == instantiation_error.

runner:ref(random_member, 2, calculate_near, 'XLOG 3.3.2').
runner:case(random_member, 2, calculate_near, 'XLOG 3.3.2, XLOG 1') :-
   random_member(X, [a, b, c]),
   contains([a, b, c], X).
runner:case(random_member, 2, calculate_near, 'XLOG 3.3.2, XLOG 2') :-
   catch(random_member(_, _), error(E, _), true),
   E == evaluation_error(undefined).

runner:ref(random_select, 3, calculate_near, 'XLOG 3.3.3').
runner:case(random_select, 3, calculate_near, 'XLOG 3.3.3, XLOG 1') :-
   random_select(X, [a, b, c], Y),
   delete([a, b, c], X, Y).
runner:case(random_select, 3, calculate_near, 'XLOG 3.3.3, XLOG 2') :-
   random_select(c, X, [a, b]),
   equal(X, [a, b, c]).
runner:case(random_select, 3, calculate_near, 'XLOG 3.3.3, XLOG 3') :-
   catch(random_select(_, _, _), error(E, _), true),
   E == evaluation_error(undefined).
