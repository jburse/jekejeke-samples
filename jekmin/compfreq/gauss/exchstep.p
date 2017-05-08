/**
 * Prolog test cases for the symbolic matrix operations.
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

% mat_cons/3
runner:ref(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1').
runner:case(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1, XLOG 1') :-
   X is [[11,77],[44,22]],
   printable(X, Y),
   Y == [[11,77],[44,22]].
runner:case(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1, XLOG 2') :-
   X is [[A,B,A-B],[A^2,B^2,(A-B)^2]],
   printable(X, Y),
   Y == [[A,B,A-B],[A^2,B^2,A^2-2*A*B+B^2]].
runner:case(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1, XLOG 3') :-
   X is {{1/(I+2*J)|between(1, 3, J)}|between(1, 3, I)},
   printable(X, Y),
   Y == [[1/3,1/5,1/7],[1/4,1/6,1/8],[1/5,1/7,1/9]].
runner:case(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1, XLOG 4') :-
   X is A,
   Y is B,
   Z is {[X^K*Y^(3-K)]|between(0, 3, K)},
   printable(Z, T),
   T == [[B^3],[A*B^2],[A^2*B],[A^3]].

% mat_at/3
runner:ref(mat_at, 3, gauss_exchstep, 'gauss 0.9.1, 2.2').
runner:case(mat_at, 3, gauss_exchstep, 'gauss 0.9.1, 2.2, XLOG 1') :-
   X is [[11,77],[44,22]],
   Y is X[1],
   printable(Y, Z),
   Z == [11,77].
runner:case(mat_at, 3, gauss_exchstep, 'gauss 0.9.1, 2.2, XLOG 2') :-
   X is [[A,B,A-B],[A^2,B^2,(A-B)^2]],
   Y is X[2],
   printable(Y, Z),
   Z == [A^2,B^2,A^2-2*A*B+B^2].

% mat_at_at/4
runner:ref(mat_at_at, 4, gauss_exchstep, 'gauss 0.9.1, 2.3').
runner:case(mat_at_at, 4, gauss_exchstep, 'gauss 0.9.1, 2.3, XLOG 1') :-
   X is [[11,77],[44,22]],
   Y is X[1,2],
   printable(Y, Z),
   Z == 77.
runner:case(mat_at_at, 4, gauss_exchstep, 'gauss 0.9.1, 2.3, XLOG 2') :-
   X is [[A,B,A-B],[A^2,B^2,(A-B)^2]],
   Y is X[2,1],
   printable(Y, Z),
   Z == A^2.

% mat_len/2
runner:ref(mat_len, 2, gauss_exchstep, 'gauss 0.9.1, 2.4').
runner:case(mat_len, 2, gauss_exchstep, 'gauss 0.9.1, 2.4, XLOG 1') :-
   X is [[11,77],[44,22]],
   Y is len(X),
   printable(Y, Z),
   Z == 2.
runner:case(mat_len, 2, gauss_exchstep, 'gauss 0.9.1, 2.4, XLOG 2') :-
   X is _,
   Y is _,
   Z is {[X^K*Y^(3-K)]|between(0, 3, K)},
   T is len(Z),
   printable(T, S),
   S == 4.

% mat_neg/2
runner:ref(mat_neg, 2, gauss_exchstep, 'gauss 0.9.1, 2.5').
runner:case(mat_neg, 2, gauss_exchstep, 'gauss 0.9.1, 2.5, XLOG 1') :-
   X is [[11,77],[-44,22]],
   Y is -X,
   printable(Y, Z),
   Z == [[-11,-77],[44,-22]].
runner:case(mat_neg, 2, gauss_exchstep, 'gauss 0.9.1, 2.5, XLOG 2') :-
   X is A,
   Y is -B,
   Z is {[X^K*Y^(3-K)]|between(0, 3, K)},
   T is -Z,
   printable(T, S),
   S == [[B^3],[-A*B^2],[A^2*B],[-A^3]].

% mat_add/3
runner:ref(mat_add, 3, gauss_exchstep, 'gauss 0.9.1, 2.6').
runner:case(mat_add, 3, gauss_exchstep, 'gauss 0.9.1, 2.6, XLOG 1') :-
   X is [[11,77],[-44,22]],
   Y is [[33,-22],[66,55]],
   Z is X+Y,
   printable(Z, T),
   T == [[44,55],[22,77]].
runner:case(mat_add, 3, gauss_exchstep, 'gauss 0.9.1, 2.6, XLOG 2') :-
   X is [[A,B,A-B]],
   Y is [[A-B,B,A]],
   Z is X+Y,
   printable(Z, T),
   T == [[2*A-B,2*B,2*A-B]].

% mat_sub/3
runner:ref(mat_sub, 3, gauss_exchstep, 'gauss 0.9.1, 2.7').
runner:case(mat_sub, 3, gauss_exchstep, 'gauss 0.9.1, 2.7, XLOG 1') :-
   X is [[11,77],[-44,22]],
   Y is [[33,-22],[66,55]],
   Z is X-Y,
   printable(Z, T),
   T == [[-22,99],[-110,-33]].
runner:case(mat_sub, 3, gauss_exchstep, 'gauss 0.9.1, 2.7, XLOG 2') :-
   X is [[A,B,A-B]],
   Y is [[A-B,B,A]],
   Z is X-Y,
   printable(Z, T),
   T == [[B,0,-B]].

% mat_mul/3
runner:ref(mat_mul, 3, gauss_exchstep, 'gauss 0.9.1, 2.8').
runner:case(mat_mul, 3, gauss_exchstep, 'gauss 0.9.1, 2.8, XLOG 1') :-
   X is [[11,77],[-44,22]],
   Y is [[33,-22],[66,55]],
   Z is X*Y,
   printable(Z, T),
   T == [[5445,3993],[0,2178]].
runner:case(mat_mul, 3, gauss_exchstep, 'gauss 0.9.1, 2.8, XLOG 2') :-
   X is [[A,B,A-B]],
   Y is [[A-B],[B],[A]],
   Z is X*Y,
   printable(Z, T),
   T == [[2*A^2-2*A*B+B^2]].

% mat_slash/3
runner:ref(mat_slash, 3, gauss_exchstep, 'gauss 0.9.1, 2.9').
runner:case(mat_slash, 3, gauss_exchstep, 'gauss 0.9.1, 2.9, XLOG 1') :-
   X is [[11,77],[-44,22]],
   Y is [[33,-22],[66,55]],
   Z is X/Y,
   printable(Z, T),
   T == [[-1-10/27,23/27],[-1-5/27,-2/27]].
runner:case(mat_slash, 3, gauss_exchstep, 'gauss 0.9.1, 2.9, XLOG 2') :-
   X is [[1,1/2],[1,1]],
   Y is [[1,1/B],[1,1]],
   Z is X/Y,
   printable(Z, T),
   T == [[1/2-1/(2-2*B),1/2+1/(2-2*B)],[0,1]].
runner:case(mat_slash, 3, gauss_exchstep, 'gauss 0.9.2, 2.9, XLOG 3') :-
   X is [[3,2],[sqrt(3),sqrt(2)]],
   Y is X/X,
   printable(Y, Z),
   Z == [[1,0],[0,1]].

% mat_int_pow/3
runner:ref(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10').
runner:case(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10, XLOG 1') :-
   X is [[1,1/_],[1,1]],
   Y is X^0,
   printable(Y, Z),
   Z == [[1,0],[0,1]].
runner:case(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10, XLOG 2') :-
   X is [[1,1/B],[1,1]],
   Y is X^3,
   printable(Y, Z),
   Z == [[1+3/B,(1+3*B)/B^2],[3+1/B,1+3/B]].
runner:case(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10, XLOG 3') :-
   X is [[11,77],[-44,22]],
   Y is X^ -1,
   printable(Y, Z),
   Z == [[1/165,-7/330],[2/165,1/330]].
runner:case(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10, XLOG 4') :-
   X is [[1,1/B],[1,1]],
   Y is X^ -1,
   printable(Y, Z),
   Z == [[1-1/(1-B),1/(1-B)],[-1+1/(1-B),1-1/(1-B)]].
