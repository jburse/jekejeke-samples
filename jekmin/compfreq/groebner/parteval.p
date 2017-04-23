/**
 * Prolog test cases for the symbolic fractions partial evaluation.
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

% eval_neg/2
runner:ref(eval_neg, 2, groebner_parteval, 'groebner 0.9.1, 1.1').
runner:case(eval_neg, 2, groebner_parteval, 'groebner 0.9.1, 1.1, XLOG 1') :-
   X is -5,
   printable(X, Y),
   Y == -5.
runner:case(eval_neg, 2, groebner_parteval, 'groebner 0.9.1, 1.1, XLOG 2') :-
   X is - (1/3),
   printable(X, Y),
   Y == -1/3.

% eval_add/3
runner:ref(eval_add, 3, groebner_parteval, 'groebner 0.9.1, 1.2').
runner:case(eval_add, 3, groebner_parteval, 'groebner 0.9.1, 1.2, XLOG 1') :-
   X is 5+8,
   printable(X, Y),
   Y == 13.
runner:case(eval_add, 3, groebner_parteval, 'groebner 0.9.1, 1.2, XLOG 2') :-
   X is 5+3/8,
   printable(X, Y),
   Y == 43/8.
runner:case(eval_add, 3, groebner_parteval, 'groebner 0.9.1, 1.2, XLOG 3') :-
   X is 5/3+8,
   printable(X, Y),
   Y == 29/3.
runner:case(eval_add, 3, groebner_parteval, 'groebner 0.9.1, 1.2, XLOG 4') :-
   X is 1/3+1/2,
   printable(X, Y),
   Y == 5/6.

% eval_sub/3
runner:ref(eval_sub, 3, groebner_parteval, 'groebner 0.9.1, 1.3').
runner:case(eval_sub, 3, groebner_parteval, 'groebner 0.9.1, 1.3, XLOG 1') :-
   X is 5-8,
   printable(X, Y),
   Y == -3.
runner:case(eval_sub, 3, groebner_parteval, 'groebner 0.9.1, 1.3, XLOG 2') :-
   X is 5-3/8,
   printable(X, Y),
   Y == 37/8.
runner:case(eval_sub, 3, groebner_parteval, 'groebner 0.9.1, 1.3, XLOG 3') :-
   X is 5/3-8,
   printable(X, Y),
   Y == -19/3.
runner:case(eval_sub, 3, groebner_parteval, 'groebner 0.9.1, 1.3, XLOG 4') :-
   X is 1/3-1/2,
   printable(X, Y),
   Y == -1/6.

% eval_mul/3
runner:ref(eval_mul, 3, groebner_parteval, 'groebner 0.9.1, 1.4').
runner:case(eval_mul, 3, groebner_parteval, 'groebner 0.9.1, 1.4, XLOG 1') :-
   X is 5*8,
   printable(X, Y),
   Y == 40.
runner:case(eval_mul, 3, groebner_parteval, 'groebner 0.9.1, 1.4, XLOG 2') :-
   X is 5*(3/8),
   printable(X, Y),
   Y == 15/8.
runner:case(eval_mul, 3, groebner_parteval, 'groebner 0.9.1, 1.4, XLOG 3') :-
   X is 5/ -3*8,
   printable(X, Y),
   Y == -40/3.
runner:case(eval_mul, 3, groebner_parteval, 'groebner 0.9.1, 1.4, XLOG 4') :-
   X is -1/3*(-1/2),
   printable(X, Y),
   Y == 1/6.

% eval_slash/3
runner:ref(eval_slash, 3, groebner_parteval, 'groebner 0.9.1, 1.5').
runner:case(eval_slash, 3, groebner_parteval, 'groebner 0.9.1, 1.5, XLOG 1') :-
   X is 5/8,
   printable(X, Y),
   Y == 5/8.
runner:case(eval_slash, 3, groebner_parteval, 'groebner 0.9.1, 1.5, XLOG 2') :-
   X is 5/(3/8),
   printable(X, Y),
   Y == 40/3.
runner:case(eval_slash, 3, groebner_parteval, 'groebner 0.9.1, 1.5, XLOG 3') :-
   X is 5/3/ -8,
   printable(X, Y),
   Y == -5/24.
runner:case(eval_slash, 3, groebner_parteval, 'groebner 0.9.1, 1.5, XLOG 4') :-
   X is -1/3/(1/ -2),
   printable(X, Y),
   Y == 2/3.

% eval_int_pow/3
runner:ref(eval_int_pow, 3, groebner_parteval, 'groebner 0.9.1, 1.6').
runner:case(eval_int_pow, 3, groebner_parteval, 'groebner 0.9.1, 1.6, XLOG 1') :-
   X is 5^2,
   printable(X, Y),
   Y == 25.
runner:case(eval_int_pow, 3, groebner_parteval, 'groebner 0.9.1, 1.6, XLOG 2') :-
   X is (1/3)^2,
   printable(X, Y),
   Y == 1/9.
