/**
 * Prolog test cases for the symbolic substitution.
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

% subst_neg/2
runner:ref(subst_neg, 2, leibniz_varchange, 'leibniz 0.9.1, 3.1').
runner:case(subst_neg, 2, leibniz_varchange, 'leibniz 0.9.1, 3.1, XLOG 1') :-
   X is subst(-A/10,A,5),
   printable(X, Y),
   Y == -1/2.
runner:case(subst_neg, 2, leibniz_varchange, 'leibniz 0.9.1, 3.1, XLOG 2') :-
   X is subst(-A,A,1/B),
   printable(X, Y),
   Y == -1/B.

% subst_add/3
runner:ref(subst_add, 3, leibniz_varchange, 'leibniz 0.9.1, 3.2').
runner:case(subst_add, 3, leibniz_varchange, 'leibniz 0.9.1, 3.2, XLOG 1') :-
   X is subst(A+A^2,A,5),
   printable(X, Y),
   Y == 30.
runner:case(subst_add, 3, leibniz_varchange, 'leibniz 0.9.1, 3.2, XLOG 2') :-
   X is subst(A+A^2,A,B+1),
   printable(X, Y),
   Y == 2+3*B+B^2.

% subst_sub/3
runner:ref(subst_sub, 3, leibniz_varchange, 'leibniz 0.9.1, 3.3').
runner:case(subst_sub, 3, leibniz_varchange, 'leibniz 0.9.1, 3.3, XLOG 1') :-
   X is subst(A-A^2,A,5),
   printable(X, Y),
   Y == -20.
runner:case(subst_sub, 3, leibniz_varchange, 'leibniz 0.9.1, 3.3, XLOG 2') :-
   X is subst(A-A^2,A,B+1),
   printable(X, Y),
   Y == -B-B^2.

% subst_mul/3
runner:ref(subst_mul, 3, leibniz_varchange, 'leibniz 0.9.1, 3.4').
runner:case(subst_mul, 3, leibniz_varchange, 'leibniz 0.9.1, 3.4, XLOG 1') :-
   X is subst((A-1)*(B-1)*(C-1),B,3),
   printable(X, Y),
   Y == 2-2*A-(2-2*A)*C.
runner:case(subst_mul, 3, leibniz_varchange, 'leibniz 0.9.1, 3.4, XLOG 2') :-
   X is subst((A-1)*(B-1)*(C-1),B,B+1),
   printable(X, Y),
   Y == (1-A)*B-(1-A)*B*C.

% subst_slash/3
runner:ref(subst_slash, 3, leibniz_varchange, 'leibniz 0.9.1, 3.5').
runner:case(subst_slash, 3, leibniz_varchange, 'leibniz 0.9.1, 3.5, XLOG 1') :-
   X is subst((A-1)*(B-1)/(C-1),B,3),
   printable(X, Y),
   Y == (2-2*A)/(1-C).
runner:case(subst_slash, 3, leibniz_varchange, 'leibniz 0.9.1, 3.5, XLOG 2') :-
   X is subst((A-1)/(B-1)*(C-1),B,B+1),
   printable(X, Y),
   Y == (1-A-(1-A)*C)/B.

% subst_int_pow/3
runner:ref(subst_int_pow, 3, leibniz_varchange, 'leibniz 0.9.1, 3.6').
runner:case(subst_int_pow, 3, leibniz_varchange, 'leibniz 0.9.1, 3.6, XLOG 1') :-
   X is subst((B-1)^3,B,3),
   printable(X, Y),
   Y == 8.
runner:case(subst_int_pow, 3, leibniz_varchange, 'leibniz 0.9.1, 3.6, XLOG 2') :-
   X is subst((B-1)^3,B,B+1),
   printable(X, Y),
   Y == B^3.
runner:case(subst_int_pow, 3, leibniz_varchange, 'leibniz 0.9.2, 3.6, XLOG 3') :-
   X is subst((1+sqrt(2)+A)^2,A,-1),
   printable(X, Y),
   Y == 2.
