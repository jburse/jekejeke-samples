/**
 * Prolog test cases for the symbolic automatic differentiation.
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

% deriv_neg/2
runner:ref(deriv_neg, 2, leibniz_autodiff, 'leibniz 0.9.1, 2.1').
runner:case(deriv_neg, 2, leibniz_autodiff, 'leibniz 0.9.1, 2.1, XLOG 1') :-
   X is deriv(-A/10,A),
   printable(X, Y),
   Y == -1/10.
runner:case(deriv_neg, 2, leibniz_autodiff, 'leibniz 0.9.1, 2.1, XLOG 2') :-
   X is deriv(-A^2,A),
   printable(X, Y),
   Y == -2*A.

% deriv_add/3
runner:ref(deriv_add, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.2').
runner:case(deriv_add, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.2, XLOG 1') :-
   X is deriv(1+A+A^2,A),
   printable(X, Y),
   Y == 1+2*A.
runner:case(deriv_add, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.2, XLOG 2') :-
   X is deriv(B*A+C*A^2,A),
   printable(X, Y),
   Y == B+2*A*C.

% deriv_sub/3
runner:ref(deriv_sub, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.3').
runner:case(deriv_sub, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.3, XLOG 1') :-
   X is deriv(1+A-A^2,A),
   printable(X, Y),
   Y == 1-2*A.
runner:case(deriv_sub, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.3, XLOG 2') :-
   X is deriv(B*A-C*A^2,A),
   printable(X, Y),
   Y == B-2*A*C.

% deriv_mul/3
runner:ref(deriv_mul, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.4').
runner:case(deriv_mul, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.4, XLOG 1') :-
   X is deriv((A-1)*(B-1)*(C-1),B),
   printable(X, Y),
   Y == 1-A-(1-A)*C.

% deriv_slash/3
runner:ref(deriv_slash, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.5').
runner:case(deriv_slash, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.5, XLOG 1') :-
   X is deriv((A-1)*(B-1)/(C-1),B),
   printable(X, Y),
   Y == (1-A)/(1-C).

% deriv_int_pow/3
runner:ref(deriv_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.6').
runner:case(deriv_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.1, 2.6, XLOG 1') :-
   X is deriv((B-1)^3,B),
   printable(X, Y),
   Y == 3-6*B+3*B^2.
