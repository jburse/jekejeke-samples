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

:- use_module(library(groebner/generic)).
:- use_module(library(misc/residue)).

% deriv_neg/2
runner:ref(deriv_neg, 2, leibniz_autodiff, 'leibniz 0.9.1, 1.1').
runner:case(deriv_neg, 2, leibniz_autodiff, 'leibniz 0.9.1, 1.1, XLOG 1') :-
   X is deriv(-A/10, A), printable(X, Y), Y == - 1/10.
runner:case(deriv_neg, 2, leibniz_autodiff, 'leibniz 0.9.1, 1.1, XLOG 2') :-
   X is deriv(-A^2, A), printable(X, Y), Y == - 2*A.

% deriv_add/3
runner:ref(deriv_add, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.2').
runner:case(deriv_add, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.2, XLOG 1') :-
   X is deriv(1+A+A^2, A), printable(X, Y), Y == 1+2*A.
runner:case(deriv_add, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.2, XLOG 2') :-
   X is deriv(B*A+C*A^2, A), printable(X, Y), Y == B+2*A*C.

% deriv_sub/3
runner:ref(deriv_sub, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.3').
runner:case(deriv_sub, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.3, XLOG 1') :-
   X is deriv(1+A-A^2, A), printable(X, Y), Y == 1-2*A.
runner:case(deriv_sub, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.3, XLOG 2') :-
   X is deriv(B*A-C*A^2, A), printable(X, Y), Y == B-2*A*C.

% deriv_mul/3
runner:ref(deriv_mul, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.4').
runner:case(deriv_mul, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.4, XLOG 1') :-
   X is deriv((B-1)*(B+1), B), printable(X, Y), Y == 2*B.
runner:case(deriv_mul, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.4, XLOG 2') :-
   X is deriv((A-1)*(B-1)*(C-1), B), printable(X, Y), Y == 1-A-(1-A)*C.

% deriv_int_pow/3
runner:ref(deriv_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.5').
runner:case(deriv_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.5, XLOG 1') :-
   X is deriv((B-1)^3, B), printable(X, Y), Y == 3-6*B+3*B^2.
runner:case(deriv_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.5, XLOG 2') :-
   X is deriv((1+sqrt(2)+A)^2, A), printable(X, Y), Y == 2+sqrt(8)+2*A.

% deriv_slash/3
runner:ref(deriv_slash, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.6').
runner:case(deriv_slash, 3, leibniz_autodiff, 'leibniz 0.9.1, 1.6, XLOG 1') :-
   X is deriv((A-1)*(B-1)/(C-1), B), printable(X, Y), Y == (1-A)/(1-C).
runner:case(deriv_slash, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.6, XLOG 2') :-
   X is deriv((A-1)/(B-1)*(C-1), B), printable(X, Y),
   Y == - (1-A-(1-A)*C)/(1-2*B+B^2).

% integ_neg/2
runner:ref(integ_neg, 2, leibniz_autodiff, 'leibniz 0.9.2, 1.7').
runner:case(integ_neg, 2, leibniz_autodiff, 'leibniz 0.9.2, 1.7, XLOG 1') :-
   X is integ(-7, B), printable(X, Y), Y == - 7*B.
runner:case(integ_neg, 2, leibniz_autodiff, 'leibniz 0.9.2, 1.7, XLOG 2') :-
   X is integ(-B^3, B), printable(X, Y), Y == - 1/4*B^4.

% integ_add/3
runner:ref(integ_add, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.8').
runner:case(integ_add, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.8, XLOG 1') :-
   X is integ(B^2+7, B), printable(X, Y), Y == 7*B+1/3*B^3.
runner:case(integ_add, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.8, XLOG 2') :-
   X is integ((A+1)*(B+1)*(C+1), B), printable(X, Y),
   Y == (1+A)*B+(1/2+1/2*A)*B^2+((1+A)*B+(1/2+1/2*A)*B^2)*C.

% integ_sub/3
runner:ref(integ_sub, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.9').
runner:case(integ_sub, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.9, XLOG 1') :-
   X is integ(B^2-7, B), printable(X, Y), Y == - 7*B+1/3*B^3.
runner:case(integ_sub, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.9, XLOG 2') :-
   X is integ((A-1)*(B-1)*(C-1), B), printable(X, Y),
   Y == - (1-A)*B+(1/2-1/2*A)*B^2+((1-A)*B-(1/2-1/2*A)*B^2)*C.

% integ_int_pow/3
runner:ref(integ_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.10').
runner:case(integ_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.10, XLOG 1') :-
   X is integ((1+B)^3, B), printable(X, Y), Y == B+(1+1/2)*B^2+B^3+1/4*B^4.
runner:case(integ_int_pow, 3, leibniz_autodiff, 'leibniz 0.9.2, 1.10, XLOG 2') :-
   X is integ((1+A*B*C)^3, B), printable(X, Y),
   Y == B+(1+1/2)*A*B^2*C+A^2*B^3*C^2+1/4*A^3*B^4*C^3.
