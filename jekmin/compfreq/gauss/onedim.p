/**
 * Prolog test cases for the symbolic vector operations.
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
:- use_module(library(advanced/arith)).
:- use_module(library(misc/residue)).

% vec_cons/3
runner:ref(vec_cons, 3, gauss_onedim, 'gauss 0.9.1, 1.1').
runner:case(vec_cons, 3, gauss_onedim, 'gauss 0.9.1, 1.1, XLOG 1') :-
   X is [11,77,44],
   printable(X, Y),
   Y == [11,77,44].
runner:case(vec_cons, 3, gauss_onedim, 'gauss 0.9.1, 1.1, XLOG 2') :-
   X is {I*I|between(1, 10, I)},
   printable(X, Y),
   Y == [1,4,9,16,25,36,49,64,81,100].
runner:case(vec_cons, 3, gauss_onedim, 'gauss 0.9.1, 1.1, XLOG 3') :-
   X is [A,B,A-B,B-A],
   printable(X, Y),
   Y == [A,B,A-B,-A+B].
runner:case(vec_cons, 3, gauss_onedim, 'gauss 0.9.1, 1.1, XLOG 4') :-
   X is 1/A,
   Y is {X^I|between(1, 5, I)},
   printable(Y, Z),
   Z == [1/A,1/A^2,1/A^3,1/A^4,1/A^5].

% vec_at/3
runner:ref(vec_at, 3, gauss_onedim, 'gauss 0.9.1, 1.2').
runner:case(vec_at, 3, gauss_onedim, 'gauss 0.9.1, 1.2, XLOG 1') :-
   X is [11,77,44],
   Y is X[2],
   printable(Y, Z),
   Z == 77.
runner:case(vec_at, 3, gauss_onedim, 'gauss 0.9.1, 1.2, XLOG 2') :-
   X is [A,B,A-B,B-A],
   Y is X[4],
   printable(Y, Z),
   Z == -A+B.

% vec_len/2
runner:ref(vec_len, 2, gauss_onedim, 'gauss 0.9.1, 1.3').
runner:case(vec_len, 2, gauss_onedim, 'gauss 0.9.1, 1.3, XLOG 1') :-
   X is [11,77,44],
   Y is len(X),
   printable(Y, Z),
   Z == 3.
runner:case(vec_len, 2, gauss_onedim, 'gauss 0.9.1, 1.3, XLOG 2') :-
   X is [A,B,A-B,B-A],
   Y is len(X),
   printable(Y, Z),
   Z == 4.

% vec_sum/2
runner:ref(vec_sum, 2, gauss_onedim, 'gauss 0.9.1, 1.4').
runner:case(vec_sum, 2, gauss_onedim, 'gauss 0.9.1, 1.4, XLOG 1') :-
   X is [11,77,44],
   Y is sum(X),
   printable(Y, Z),
   Z == 132.
runner:case(vec_sum, 2, gauss_onedim, 'gauss 0.9.1, 1.4, XLOG 2') :-
   X is [A,B,A-B,B-A],
   Y is sum(X),
   printable(Y, Z),
   Z == A+B.
runner:case(vec_sum, 2, gauss_onedim, 'gauss 0.9.2, 1.4, XLOG 3') :-
   X is sum([sqrt(3),sqrt(2)]),
   printable(X, Y),
   Y == sqrt(2)+sqrt(3).

% vec_min/2
runner:ref(vec_min, 2, gauss_onedim, 'gauss 0.9.1, 1.5').
runner:case(vec_min, 2, gauss_onedim, 'gauss 0.9.1, 1.5, XLOG 1') :-
   X is min([5,3,7]),
   printable(X, Y),
   Y == 3.
runner:case(vec_min, 2, gauss_onedim, 'gauss 0.9.1, 1.5, XLOG 2') :-
   X is min([9/5,17/7,6/5]),
   printable(X, Y),
   Y == 1+1/5.

% vec_max/2
runner:ref(vec_max, 2, gauss_onedim, 'gauss 0.9.1, 1.6').
runner:case(vec_max, 2, gauss_onedim, 'gauss 0.9.1, 1.6, XLOG 1') :-
   X is max([5,3,7]),
   printable(X, Y),
   Y == 7.
runner:case(vec_max, 2, gauss_onedim, 'gauss 0.9.1, 1.6, XLOG 2') :-
   X is max([9/5,17/7,6/5]),
   printable(X, Y),
   Y == 2+3/7.

% vec_neg/2
runner:ref(vec_neg, 2, gauss_onedim, 'gauss 0.9.1, 1.7').
runner:case(vec_neg, 2, gauss_onedim, 'gauss 0.9.1, 1.7, XLOG 1') :-
   X is [11,77,44],
   Y is -X,
   printable(Y, Z),
   Z == [-11,-77,-44].
runner:case(vec_neg, 2, gauss_onedim, 'gauss 0.9.1, 1.7, XLOG 2') :-
   X is [A,B,A-B,B-A],
   Y is -X,
   printable(Y, Z),
   Z == [-A,-B,-A+B,A-B].

% vec_add/3
runner:ref(vec_add, 3, gauss_onedim, 'gauss 0.9.1, 1.8').
runner:case(vec_add, 3, gauss_onedim, 'gauss 0.9.1, 1.8, XLOG 1') :-
   X is [3/10,1/2,1/5],
   Y is [3/5,7/10,1/10],
   Z is X+Y,
   printable(Z, T),
   T == [9/10,1+1/5,3/10].
runner:case(vec_add, 3, gauss_onedim, 'gauss 0.9.1, 1.8, XLOG 2') :-
   X is [A,B,A-B,B-A],
   Y is [B,2*B,3*B,-B],
   Z is X+Y,
   printable(Z, T),
   T == [A+B,3*B,A+2*B,-A].

% vec_sub/3
runner:ref(vec_sub, 3, gauss_onedim, 'gauss 0.9.1, 1.9').
runner:case(vec_sub, 3, gauss_onedim, 'gauss 0.9.1, 1.9, XLOG 1') :-
   X is [3/10,1/2,1/5],
   Y is [3/5,7/10,1/10],
   Z is X-Y,
   printable(Z, T),
   T == [-3/10,-1/5,1/10].
runner:case(vec_sub, 3, gauss_onedim, 'gauss 0.9.1, 1.9, XLOG 2') :-
   X is [A,B,A-B,B-A],
   Y is [B,2*B,3*B,-B],
   Z is X-Y,
   printable(Z, T),
   T == [A-B,-B,A-4*B,-A+2*B].
