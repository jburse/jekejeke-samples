/**
 * Prolog test cases for the symbolic fractions simplification.
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

:- use_module(library(groebner/generic)).
:- use_module(library(misc/residue)).

% simp_neg/2
runner:ref(simp_neg, 2, groebner_simplify, 'groebner 0.9.1, 2.1').
runner:case(simp_neg, 2, groebner_simplify, 'groebner 0.9.1, 2.1, XLOG 1') :-
   X is -A,
   printable(X, Y),
   Y == -A.
runner:case(simp_neg, 2, groebner_simplify, 'groebner 0.9.1, 2.1, XLOG 2') :-
   X is - (A^2-3*A+4),
   printable(X, Y),
   Y == -4+3*A-A^2.
runner:case(simp_neg, 2, groebner_simplify, 'groebner 0.9.1, 2.1, XLOG 3') :-
   X is - (A-7)/(B-3),
   printable(X, Y),
   Y == - (7-A)/(3-B).
runner:case(simp_neg, 2, groebner_simplify, 'groebner 0.9.2, 2.1, XLOG 4') :-
   X is A/(sqrt(2)-sqrt(3)),
   printable(X, Y),
   Y == - (sqrt(2)+sqrt(3))*A.

% simp_add/3
runner:ref(simp_add, 3, groebner_simplify, 'groebner 0.9.1, 2.2').
runner:case(simp_add, 3, groebner_simplify, 'groebner 0.9.1, 2.2, XLOG 1') :-
   X is 2*A+A,
   printable(X, Y),
   Y == 3*A.
runner:case(simp_add, 3, groebner_simplify, 'groebner 0.9.1, 2.2, XLOG 2') :-
   X is A+B,
   printable(X, Y),
   Y == A+B.
runner:case(simp_add, 3, groebner_simplify, 'groebner 0.9.1, 2.2, XLOG 3') :-
   X is A+B/A,
   printable(X, Y),
   Y == A+B/A.
runner:case(simp_add, 3, groebner_simplify, 'groebner 0.9.1, 2.2, XLOG 4') :-
   X is A*(B-1)+B*(A+2),
   printable(X, Y),
   Y == -A+(2+2*A)*B.
runner:case(simp_add, 3, groebner_simplify, 'groebner 0.9.1, 2.2, XLOG 5') :-
   X is 7/(B-3)+(B+2)/5,
   printable(X, Y),
   Y == 2/5+1/5*B-7/(3-B).
runner:case(simp_add, 3, groebner_simplify, 'groebner 0.9.2, 2.2, XLOG 6') :-
   X is (A+sqrt(2))*(A+sqrt(8)),
   printable(X, Y),
   Y == 4+sqrt(18)*A+A^2.

% simp_sub/3
runner:ref(simp_sub, 3, groebner_simplify, 'groebner 0.9.1, 2.3').
runner:case(simp_sub, 3, groebner_simplify, 'groebner 0.9.1, 2.3, XLOG 1') :-
   X is A-2*A,
   printable(X, Y),
   Y == -A.
runner:case(simp_sub, 3, groebner_simplify, 'groebner 0.9.1, 2.3, XLOG 2') :-
   X is A-B,
   printable(X, Y),
   Y == A-B.
runner:case(simp_sub, 3, groebner_simplify, 'groebner 0.9.1, 2.3, XLOG 3') :-
   X is A-A/B,
   printable(X, Y),
   Y == A-A/B.
runner:case(simp_sub, 3, groebner_simplify, 'groebner 0.9.1, 2.3, XLOG 4') :-
   X is A*(B-1)-B*(A+2),
   printable(X, Y),
   Y == -A-2*B.
runner:case(simp_sub, 3, groebner_simplify, 'groebner 0.9.1, 2.3, XLOG 5') :-
   X is 7/(B-3)-(B+2)/5,
   printable(X, Y),
   Y == -2/5-1/5*B-7/(3-B).
runner:case(simp_sub, 3, groebner_simplify, 'groebner 0.9.2, 2.3, XLOG 6') :-
   X is (A-sqrt(2))*(A-sqrt(8)),
   printable(X, Y),
   Y == 4-sqrt(18)*A+A^2.

% simp_mul/3
runner:ref(simp_mul, 3, groebner_simplify, 'groebner 0.9.1, 2.4').
runner:case(simp_mul, 3, groebner_simplify, 'groebner 0.9.1, 2.4, XLOG 1') :-
   X is A^2*A,
   printable(X, Y),
   Y == A^3.
runner:case(simp_mul, 3, groebner_simplify, 'groebner 0.9.1, 2.4, XLOG 2') :-
   X is A*(A+1),
   printable(X, Y),
   Y == A+A^2.
runner:case(simp_mul, 3, groebner_simplify, 'groebner 0.9.1, 2.4, XLOG 3') :-
   X is A*B/A,
   printable(X, Y),
   Y == B.
runner:case(simp_mul, 3, groebner_simplify, 'groebner 0.9.1, 2.4, XLOG 4') :-
   X is (B-1)*(A*B+A),
   printable(X, Y),
   Y == - (1-B^2)*A.
runner:case(simp_mul, 3, groebner_simplify, 'groebner 0.9.1, 2.4, XLOG 5') :-
   X is (B-1)/A*((B+1)/A),
   printable(X, Y),
   Y == - (1-B^2)/A^2.
runner:case(simp_mul, 3, groebner_simplify, 'groebner 0.9.2, 2.4, XLOG 6') :-
   X is sqrt(1/5)*(sqrt(10)/A+sqrt(15)/B),
   printable(X, Y),
   Y == (sqrt(3)*A+sqrt(2)*B)/(A*B).

% simp_slash/3
runner:ref(simp_slash, 3, groebner_simplify, 'groebner 0.9.1, 2.5').
runner:case(simp_slash, 3, groebner_simplify, 'groebner 0.9.1, 2.5, XLOG 1') :-
   X is A/A^2,
   printable(X, Y),
   Y == 1/A.
runner:case(simp_slash, 3, groebner_simplify, 'groebner 0.9.1, 2.5, XLOG 2') :-
   X is A/(A+1),
   printable(X, Y),
   Y == 1-1/(1+A).
runner:case(simp_slash, 3, groebner_simplify, 'groebner 0.9.1, 2.5, XLOG 3') :-
   X is A/(A/B),
   printable(X, Y),
   Y == B.
runner:case(simp_slash, 3, groebner_simplify, 'groebner 0.9.1, 2.5, XLOG 4') :-
   X is (B^2-1)/(A*B+A),
   printable(X, Y),
   Y == - (1-B)/A.
runner:case(simp_slash, 3, groebner_simplify, 'groebner 0.9.1, 2.5, XLOG 5') :-
   X is (B^2-1)/A/((A*B+A)/A),
   printable(X, Y),
   Y == - (1-B)/A.
runner:case(simp_slash, 3, groebner_simplify, 'groebner 0.9.2, 2.5, XLOG 6') :-
   X is (sqrt(10)/A+sqrt(15)/B)/sqrt(5),
   printable(X, Y),
   Y == (sqrt(3)*A+sqrt(2)*B)/(A*B).

% simp_int_pow/3
runner:ref(simp_int_pow, 3, groebner_simplify, 'groebner 0.9.1, 2.6').
runner:case(simp_int_pow, 3, groebner_simplify, 'groebner 0.9.1, 2.6, XLOG 1') :-
   X is A^2,
   printable(X, Y),
   Y == A^2.
runner:case(simp_int_pow, 3, groebner_simplify, 'groebner 0.9.1, 2.6, XLOG 2') :-
   X is (1+A)^5,
   printable(X, Y),
   Y == 1+5*A+10*A^2+10*A^3+5*A^4+A^5.
runner:case(simp_int_pow, 3, groebner_simplify, 'groebner 0.9.1, 2.6, XLOG 3') :-
   X is ((1-A)/(1+A))^3,
   printable(X, Y),
   Y == -1+(2+6*A^2)/(1+3*A+3*A^2+A^3).
runner:case(simp_int_pow, 3, groebner_simplify, 'groebner 0.9.2, 2.6, XLOG 4') :-
   X is (1+sqrt(2)+A)^2,
   printable(X, Y),
   Y == 3+sqrt(8)+(2+sqrt(8))*A+A^2.
runner:case(simp_int_pow, 3, groebner_simplify, 'groebner 0.9.2, 2.6, XLOG 5') :-
   X is (sqrt(3)*A*B-sqrt(2))*A,
   Y is (sqrt(3)*A*B-sqrt(2))*B,
   Z is (X/Y)^2,
   printable(Z, T),
   T == A^2/B^2.
