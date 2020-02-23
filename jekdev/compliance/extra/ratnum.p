/**
 * Prolog code for the extra rational number test cases.
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

:- use_module(library(arithmetic/ratio)).

/****************************************************************/
/* ratio.p extras                                               */
/****************************************************************/

/* rdiv(X, Y) */

runner:ref(rdiv, -2, extra_ratnum, 'XLOG 6.1.1').
runner:case(rdiv, -2, extra_ratnum, 'XLOG 6.1.1, XLOG 1') :-
   7#4 is 5#2 rdiv 10#7.
runner:case(rdiv, -2, extra_ratnum, 'XLOG 6.1.1, XLOG 2') :-
   -5#6 is -5#2 rdiv 3.
runner:case(rdiv, -2, extra_ratnum, 'XLOG 6.1.1, XLOG 3') :-
   14#5 is 4 rdiv 10#7.
runner:case(rdiv, -2, extra_ratnum, 'XLOG 6.1.1, XLOG 4') :-
   -2 is 12 rdiv-6.

/* #(X, Y) */

runner:ref(#, -2, extra_ratnum, 'XLOG 6.1.2').
runner:case(#, -2, extra_ratnum, 'XLOG 6.1.2, XLOG 1') :-
   X is 12# -6, X == 12# -6.

/* rational(X) */

runner:ref(rational, 1, extra_ratnum, 'XLOG 6.1.3').
runner:case(rational, 1, extra_ratnum, 'XLOG 6.1.3, XLOG 1') :-
   rational(10#7).
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.3, XLOG 2') :-
   rational(-5#2).
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.3, XLOG 3') :-
   rational(12).
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.3, XLOG 4') :-
   rational(-6).
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.3, XLOG 4') :-
   \+ rational(3.1415).

/* numerator(X) */

runner:ref(numerator, -2, extra_ratnum, 'XLOG 6.1.4').
runner:case(numerator, -2, extra_ratnum, 'XLOG 6.1.4, XLOG 1') :-
   X is numerator(10#7), X == 10.
runner:case(numerator, -2, extra_ratnum, 'XLOG 6.1.4, XLOG 2') :-
   X is numerator(-5#2), X == -5.
runner:case(numerator, -2, extra_ratnum, 'XLOG 6.1.4, XLOG 3') :-
   X is numerator(12), X == 12.
runner:case(numerator, -2, extra_ratnum, 'XLOG 6.1.4, XLOG 4') :-
   X is numerator(-6), X == -6.

/* denominator(X) */

runner:ref(denominator, -2, extra_ratnum, 'XLOG 6.1.5').
runner:case(denominator, -2, extra_ratnum, 'XLOG 6.1.5, XLOG 1') :-
   X is denominator(10#7), X == 7.
runner:case(denominator, -2, extra_ratnum, 'XLOG 6.1.5, XLOG 2') :-
   X is denominator(-5#2), X == 2.
runner:case(denominator, -2, extra_ratnum, 'XLOG 6.1.5, XLOG 3') :-
   X is denominator(12), X == 1.
runner:case(denominator, -2, extra_ratnum, 'XLOG 6.1.5, XLOG 4') :-
   X is denominator(-6), X == 1.

/* rational(X, N, D) */

runner:ref(rational, 3, extra_ratnum, 'XLOG 6.1.6').
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.6, XLOG 1') :-
   rational(10#7, N, D), N == 10, D == 7.
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.6, XLOG 2') :-
   rational(-5#2, N, D), N == -5, D == 2.
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.6, XLOG 3') :-
   rational(12, N, D), N == 12, D == 1.
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.6, XLOG 4') :-
   rational(-6, N, D), N == -6, D == 1.
runner:case(rational, 3, extra_ratnum, 'XLOG 6.1.6, XLOG 5') :-
   \+ rational(3.1415, _, _).

/****************************************************************/
/* ratio.p elem.p extras                                        */
/****************************************************************/

/* -(X) */

runner:ref(-, -2, extra_ratnum, 'XLOG 6.2.1').
runner:case(-, -2, extra_ratnum, 'XLOG 6.2.1, XLOG 1') :-
   X is - 10#7, X == -10#7.
runner:case(-, -2, extra_ratnum, 'XLOG 6.2.1, XLOG 2') :-
   X is - -5#2, X == 5#2.
runner:case(-, -2, extra_ratnum, 'XLOG 6.2.1, XLOG 3') :-
   X is - 5#2, X == -5#2.
runner:case(-, -2, extra_ratnum, 'XLOG 6.2.1, XLOG 4') :-
   X is - -6, X == 6.

/* +(X) */

runner:ref(+, -2, extra_ratnum, 'XLOG 6.2.2').
runner:case(+, -2, extra_ratnum, 'XLOG 6.2.2, XLOG 1') :-
   X is +10#7, X == 10#7.
runner:case(+, -2, extra_ratnum, 'XLOG 6.2.2, XLOG 2') :-
   X is + -5#2, X == -5#2.
runner:case(+, -2, extra_ratnum, 'XLOG 6.2.2, XLOG 3') :-
   X is +5#2, X == 5#2.
runner:case(+, -2, extra_ratnum, 'XLOG 6.2.2, XLOG 4') :-
   X is + -6, X == -6.

/* abs(X) */

runner:ref(abs, -2, extra_ratnum, 'XLOG 6.2.3').
runner:case(abs, -2, extra_ratnum, 'XLOG 6.2.3, XLOG 1') :-
   X is abs(10#7), X == 10#7.
runner:case(abs, -2, extra_ratnum, 'XLOG 6.2.3, XLOG 2') :-
   X is abs(-5#2), X == 5#2.
runner:case(abs, -2, extra_ratnum, 'XLOG 6.2.3, XLOG 3') :-
   X is abs(5#2), X == 5#2.
runner:case(abs, -2, extra_ratnum, 'XLOG 6.2.3, XLOG 4') :-
   X is abs(-6), X == 6.

/* sign(X) */

runner:ref(sign, -2, extra_ratnum, 'XLOG 6.2.4').
runner:case(sign, -2, extra_ratnum, 'XLOG 6.2.4, XLOG 1') :-
   X is sign(10#7), X == 1.
runner:case(sign, -2, extra_ratnum, 'XLOG 6.2.4, XLOG 2') :-
   X is sign(-5#2), X == -1.
runner:case(sign, -2, extra_ratnum, 'XLOG 6.2.4, XLOG 3') :-
   X is sign(5#2), X == 1.
runner:case(sign, -2, extra_ratnum, 'XLOG 6.2.4, XLOG 4') :-
   X is sign(-6), X == -1.

/* float(X) */

runner:ref(float, -2, extra_ratnum, 'XLOG 6.2.5').
runner:case(float, -2, extra_ratnum, 'XLOG 6.2.5, XLOG 1') :-
   X is float(10#7), X == 1.4285714285714286.
runner:case(float, -2, extra_ratnum, 'XLOG 6.2.5, XLOG 2') :-
   X is float(-5#2), X == -2.5.
runner:case(float, -2, extra_ratnum, 'XLOG 6.2.5, XLOG 3') :-
   X is float(5#2), X == 2.5.
runner:case(float, -2, extra_ratnum, 'XLOG 6.2.5, XLOG 4') :-
   X is float(-6), X == -6.0.

/* decimal(X) */

runner:ref(decimal, -2, extra_ratnum, 'XLOG 6.2.6').
runner:case(decimal, -2, extra_ratnum, 'XLOG 6.2.6, XLOG 1') :-
   true.

/* float32(X) */

runner:ref(float32, -2, extra_ratnum, 'XLOG 6.2.7').
runner:case(float32, -2, extra_ratnum, 'XLOG 6.2.7, XLOG 1') :-
   X is float32(10#7), X == 0f1.4285715.
runner:case(float32, -2, extra_ratnum, 'XLOG 6.2.7, XLOG 1') :-
   X is float32(-5#2), X == -0f2.5.
runner:case(float32, -2, extra_ratnum, 'XLOG 6.2.7, XLOG 1') :-
   X is float32(5#2), X == 0f2.5.
runner:case(float32, -2, extra_ratnum, 'XLOG 6.2.7, XLOG 1') :-
   X is float32(-6), X == -0f6.0.

/* +(X, Y) */

runner:ref(+, -3, extra_ratnum, 'XLOG 6.2.8').
runner:case(+, -3, extra_ratnum, 'XLOG 6.2.8, XLOG 1') :-
   X is 5#2+10#7, X == 55#14.
runner:case(+, -3, extra_ratnum, 'XLOG 6.2.8, XLOG 2') :-
   X is -5#2+3, X == 1#2.
runner:case(+, -3, extra_ratnum, 'XLOG 6.2.8, XLOG 3') :-
   X is 4+10#7, X == 38#7.
runner:case(+, -3, extra_ratnum, 'XLOG 6.2.8, XLOG 4') :-
   X is 12+ -6, X == 6.

/* -(X, Y) */

runner:ref(-, -3, extra_ratnum, 'XLOG 6.2.9').
runner:case(-, -3, extra_ratnum, 'XLOG 6.2.9, XLOG 1') :-
   X is 5#2-10#7, X == 15#14.
runner:case(-, -3, extra_ratnum, 'XLOG 6.2.9, XLOG 2') :-
   X is -5#2-3, X == -11#2.
runner:case(-, -3, extra_ratnum, 'XLOG 6.2.9, XLOG 3') :-
   X is 4-10#7, X == 18#7.
runner:case(-, -3, extra_ratnum, 'XLOG 6.2.9, XLOG 4') :-
   X is 12- -6, X == 18.

/* *(X, Y) */

runner:ref(*, -3, extra_ratnum, 'XLOG 6.2.10').
runner:case(*, -3, extra_ratnum, 'XLOG 6.2.10, XLOG 1') :-
   X is 5#2*10#7, X == 25#7.
runner:case(*, -3, extra_ratnum, 'XLOG 6.2.10, XLOG 2') :-
   X is -5#2*3, X == -15#2.
runner:case(*, -3, extra_ratnum, 'XLOG 6.2.10, XLOG 3') :-
   X is 4*10#7, X == 40#7.
runner:case(*, -3, extra_ratnum, 'XLOG 6.2.10, XLOG 4') :-
   X is 12* -6, X == -72.

/* /(X, Y) */

runner:ref(/, -3, extra_ratnum, 'XLOG 6.2.11').
runner:case(/, -3, extra_ratnum, 'XLOG 6.2.11, XLOG 1') :-
   X is 5#2/10#7, X == 1.75.
runner:case(/, -3, extra_ratnum, 'XLOG 6.2.11, XLOG 2') :-
   X is -5#2/3, X == -0.8333333333333334.
runner:case(/, -3, extra_ratnum, 'XLOG 6.2.11, XLOG 3') :-
   X is 4/10#7, X == 2.8.
runner:case(/, -3, extra_ratnum, 'XLOG 6.2.11, XLOG 4') :-
   X is 12/ -6, X == -2.0.

/* ^(X, Y) */

runner:ref(^, -3, extra_ratnum, 'XLOG 6.2.12').
runner:case(^, -3, extra_ratnum, 'XLOG 6.2.12, XLOG 1') :-
   -125#8 is -5#2^3.
runner:case(^, -3, extra_ratnum, 'XLOG 6.2.12, XLOG 2') :-
   1#2985984 is 12^ -6.

/****************************************************************/
/* ratio.p bits.p extras                                        */
/****************************************************************/

/* gcd(X, Y) */

runner:ref(gcd, -3, extra_ratnum, 'XLOG 6.3.1').
runner:case(gcd, -3, extra_ratnum, 'XLOG 6.3.1, XLOG 1') :-
   5#14 is gcd(5#2, 10#7).
runner:case(gcd, -3, extra_ratnum, 'XLOG 6.3.1, XLOG 2') :-
   1#2 is gcd(-5#2, 3).
runner:case(gcd, -3, extra_ratnum, 'XLOG 6.3.1, XLOG 3') :-
   2#7 is gcd(4, 10#7).
runner:case(gcd, -3, extra_ratnum, 'XLOG 6.3.1, XLOG 4') :-
   6 is gcd(12, -6).

/* lcm(X, Y) */

runner:ref(lcm, -3, extra_ratnum, 'XLOG 6.3.2').
runner:case(lcm, -3, extra_ratnum, 'XLOG 6.3.2, XLOG 1') :-
   10 is lcm(5#2, 10#7).
runner:case(lcm, -3, extra_ratnum, 'XLOG 6.3.2, XLOG 2') :-
   -15 is lcm(-5#2, 3).
runner:case(lcm, -3, extra_ratnum, 'XLOG 6.3.2, XLOG 3') :-
   20 is lcm(4, 10#7).
runner:case(lcm, -3, extra_ratnum, 'XLOG 6.3.2, XLOG 4') :-
   -12 is lcm(12, -6).

/****************************************************************/
/* ratio.p round.p extras                                      */
/****************************************************************/

/* integer(X) */

runner:ref(integer, -2, extra_ratnum, 'XLOG 6.4.1').
runner:case(integer, -2, extra_ratnum, 'XLOG 6.4.1, XLOG 1') :-
   X is integer(10#7), X == 1.
runner:case(integer, -2, extra_ratnum, 'XLOG 6.4.1, XLOG 2') :-
   X is integer(-5#2), X == -2.
runner:case(integer, -2, extra_ratnum, 'XLOG 6.4.1, XLOG 3') :-
   X is integer(5#2), X == 2.
runner:case(integer, -2, extra_ratnum, 'XLOG 6.4.1, XLOG 4') :-
   X is integer(-6), X == -6.

/* truncate(X) */

runner:ref(truncate, -2, extra_ratnum, 'XLOG 6.4.2').
runner:case(truncate, -2, extra_ratnum, 'XLOG 6.4.2, XLOG 1') :-
   X is truncate(10#7), X == 1.
runner:case(truncate, -2, extra_ratnum, 'XLOG 6.4.2, XLOG 2') :-
   X is truncate(-5#2), X == -2.
runner:case(truncate, -2, extra_ratnum, 'XLOG 6.4.2, XLOG 3') :-
   X is truncate(5#2), X == 2.
runner:case(truncate, -2, extra_ratnum, 'XLOG 6.4.2, XLOG 4') :-
   X is truncate(-6), X == -6.

/* floor(X) */

runner:ref(floor, -2, extra_ratnum, 'XLOG 6.4.3').
runner:case(floor, -2, extra_ratnum, 'XLOG 6.4.3, XLOG 1') :-
   X is floor(10#7), X == 1.
runner:case(floor, -2, extra_ratnum, 'XLOG 6.4.3, XLOG 2') :-
   X is floor(-5#2), X == -3.
runner:case(floor, -2, extra_ratnum, 'XLOG 6.4.3, XLOG 3') :-
   X is floor(5#2), X == 2.
runner:case(floor, -2, extra_ratnum, 'XLOG 6.4.3, XLOG 4') :-
   X is floor(-6), X == -6.

/* ceiling(X) */

runner:ref(ceiling, -2, extra_ratnum, 'XLOG 6.4.4').
runner:case(ceiling, -2, extra_ratnum, 'XLOG 6.4.4, XLOG 1') :-
   X is ceiling(10#7), X == 2.
runner:case(ceiling, -2, extra_ratnum, 'XLOG 6.4.4, XLOG 2') :-
   X is ceiling(-5#2), X == -2.
runner:case(ceiling, -2, extra_ratnum, 'XLOG 6.4.4, XLOG 3') :-
   X is ceiling(5#2), X == 3.
runner:case(ceiling, -2, extra_ratnum, 'XLOG 6.4.4, XLOG 4') :-
   X is ceiling(-6), X == -6.

/* round(X) */

runner:ref(round, -2, extra_ratnum, 'XLOG 6.4.5').
runner:case(round, -2, extra_ratnum, 'XLOG 6.4.5, XLOG 1') :-
   X is round(10#7), X == 1.
runner:case(round, -2, extra_ratnum, 'XLOG 6.4.5, XLOG 2') :-
   X is round(-5#2), X == -3.
runner:case(round, -2, extra_ratnum, 'XLOG 6.4.5, XLOG 3') :-
   X is round(5#2), X == 3.
runner:case(round, -2, extra_ratnum, 'XLOG 6.4.5, XLOG 4') :-
   X is round(-6), X == -6.

/* X // Y */

runner:ref(//, -3, extra_ratnum, 'XLOG 6.4.6').
runner:case(//, -3, extra_ratnum, 'XLOG 6.4.6, XLOG 1') :-
   1 is 5#2//10#7.
runner:case(//, -3, extra_ratnum, 'XLOG 6.4.6, XLOG 2') :-
   0 is -5#2//3.
runner:case(//, -3, extra_ratnum, 'XLOG 6.4.6, XLOG 3') :-
   2 is 4//10#7.
runner:case(//, -3, extra_ratnum, 'XLOG 6.4.6, XLOG 4') :-
   -2 is 12// -6.

/* X rem Y */

runner:ref(rem, -3, extra_ratnum, 'XLOG 6.4.7').
runner:case(rem, -3, extra_ratnum, 'XLOG 6.4.7, XLOG 1') :-
   15#14 is 5#2 rem 10#7.
runner:case(rem, -3, extra_ratnum, 'XLOG 6.4.7, XLOG 2') :-
   -5#2 is -5#2 rem 3.
runner:case(rem, -3, extra_ratnum, 'XLOG 6.4.7, XLOG 3') :-
   8#7 is 4 rem 10#7.
runner:case(rem, -3, extra_ratnum, 'XLOG 6.4.7, XLOG 4') :-
   0 is 12 rem-6.

/* X div Y */

runner:ref(div, -3, extra_ratnum, 'XLOG 6.4.8').
runner:case(div, -3, extra_ratnum, 'XLOG 6.4.8, XLOG 1') :-
   1 is 5#2 div 10#7.
runner:case(div, -3, extra_ratnum, 'XLOG 6.4.8, XLOG 2') :-
   -1 is -5#2 div 3.
runner:case(div, -3, extra_ratnum, 'XLOG 6.4.8, XLOG 3') :-
   2 is 4 div 10#7.
runner:case(div, -3, extra_ratnum, 'XLOG 6.4.8, XLOG 4') :-
   -2 is 12 div-6.

/* X mod Y */

runner:ref(mod, -3, extra_ratnum, 'XLOG 6.4.9').
runner:case(mod, -3, extra_ratnum, 'XLOG 6.4.9, XLOG 1') :-
   15#14 is 5#2 mod 10#7.
runner:case(mod, -3, extra_ratnum, 'XLOG 6.4.9, XLOG 2') :-
   1#2 is -5#2 mod 3.
runner:case(mod, -3, extra_ratnum, 'XLOG 6.4.9, XLOG 3') :-
   8#7 is 4 mod 10#7.
runner:case(mod, -3, extra_ratnum, 'XLOG 6.4.9, XLOG 4') :-
   0 is 12 mod-6.

/* divmod(X,Y,D,M) */

runner:ref(divmod, 4, extra_ratnum, 'XLOG 6.4.10').
runner:case(divmod, 4, extra_ratnum, 'XLOG 6.4.10, XLOG 1') :-
   divmod(5#2, 10#7, D, M), D == 1, M == 15#14.
runner:case(divmod, 4, extra_ratnum, 'XLOG 6.4.10, XLOG 2') :-
   divmod(-5#2, 3, D, M), D == -1, M == 1#2.
runner:case(divmod, 4, extra_ratnum, 'XLOG 6.4.10, XLOG 3') :-
   divmod(4, 10#7, D, M), D == 2, M == 8#7.
runner:case(divmod, 4, extra_ratnum, 'XLOG 6.4.10, XLOG 4') :-
   divmod(12, -6, D, M), D == -2, M == 0.

/****************************************************************/
/* ratio.p compare.p extras                                    */
/****************************************************************/

/* X =:= Y */

runner:ref(=:=, 2, extra_ratnum, 'XLOG 6.5.1').
runner:case(=:=, 2, extra_ratnum, 'XLOG 6.5.1, XLOG 1') :-
   \+ 5#2 =:= 10#7.
runner:case(=:=, 2, extra_ratnum, 'XLOG 6.5.1, XLOG 2') :-
   \+ -5#2 =:= 3.
runner:case(=:=, 2, extra_ratnum, 'XLOG 6.5.1, XLOG 3') :-
   \+ 4 =:= 10#7.
runner:case(=:=, 2, extra_ratnum, 'XLOG 6.5.1, XLOG 4') :-
   \+ 12 =:= -6.
runner:case(=:=, 2, extra_ratnum, 'XLOG 6.5.1, XLOG 5') :-
   2#3 =:= 2#3.
runner:case(=:=, 2, extra_ratnum, 'XLOG 6.5.1, XLOG 6') :-
   -10 =:= -10.

/* X =\= Y */

runner:ref(=\=, 2, extra_ratnum, 'XLOG 6.5.2').
runner:case(=\=, 2, extra_ratnum, 'XLOG 6.5.2, XLOG 1') :-
   5#2 =\= 10#7.
runner:case(=\=, 2, extra_ratnum, 'XLOG 6.5.2, XLOG 2') :-
   -5#2 =\= 3.
runner:case(=\=, 2, extra_ratnum, 'XLOG 6.5.2, XLOG 3') :-
   4 =\= 10#7.
runner:case(=\=, 2, extra_ratnum, 'XLOG 6.5.2, XLOG 4') :-
   12 =\= -6.
runner:case(=\=, 2, extra_ratnum, 'XLOG 6.5.2, XLOG 5') :-
   \+ 2#3 =\= 2#3.
runner:case(=\=, 2, extra_ratnum, 'XLOG 6.5.2, XLOG 6') :-
   \+ -10 =\= -10.

/* X < Y */

runner:ref(<, 2, extra_ratnum, 'XLOG 6.5.3').
runner:case(<, 2, extra_ratnum, 'XLOG 6.5.3, XLOG 1') :-
   \+ 5#2 < 10#7.
runner:case(<, 2, extra_ratnum, 'XLOG 6.5.3, XLOG 2') :-
   -5#2 < 3.
runner:case(<, 2, extra_ratnum, 'XLOG 6.5.3, XLOG 3') :-
   \+ 4 < 10#7.
runner:case(<, 2, extra_ratnum, 'XLOG 6.5.3, XLOG 4') :-
   \+ 12 < -6.
runner:case(<, 2, extra_ratnum, 'XLOG 6.5.3, XLOG 5') :-
   \+ 2#3 < 2#3.
runner:case(<, 2, extra_ratnum, 'XLOG 6.5.3, XLOG 6') :-
   \+ -10 < -10.

/* X =< Y */

runner:ref(=<, 2, extra_ratnum, 'XLOG 6.5.4').
runner:case(=<, 2, extra_ratnum, 'XLOG 6.5.4, XLOG 1') :-
   \+ 5#2 =< 10#7.
runner:case(=<, 2, extra_ratnum, 'XLOG 6.5.4, XLOG 2') :-
   -5#2 =< 3.
runner:case(=<, 2, extra_ratnum, 'XLOG 6.5.4, XLOG 3') :-
   \+ 4 =< 10#7.
runner:case(=<, 2, extra_ratnum, 'XLOG 6.5.4, XLOG 4') :-
   \+ 12 =< -6.
runner:case(=<, 2, extra_ratnum, 'XLOG 6.5.4, XLOG 5') :-
   2#3 =< 2#3.
runner:case(=<, 2, extra_ratnum, 'XLOG 6.5.4, XLOG 6') :-
   -10 =< -10.

/* X > Y */

runner:ref(>, 2, extra_ratnum, 'XLOG 6.5.5').
runner:case(>, 2, extra_ratnum, 'XLOG 6.5.5, XLOG 1') :-
   5#2 > 10#7.
runner:case(>, 2, extra_ratnum, 'XLOG 6.5.5, XLOG 2') :-
   \+ -5#2 > 3.
runner:case(>, 2, extra_ratnum, 'XLOG 6.5.5, XLOG 3') :-
   4 > 10#7.
runner:case(>, 2, extra_ratnum, 'XLOG 6.5.5, XLOG 4') :-
   12 > -6.
runner:case(>, 2, extra_ratnum, 'XLOG 6.5.5, XLOG 5') :-
   \+ 2#3 > 2#3.
runner:case(>, 2, extra_ratnum, 'XLOG 6.5.5, XLOG 6') :-
   \+ -10 > -10.

/* X >= Y */

runner:ref(>=, 2, extra_ratnum, 'XLOG 6.5.6').
runner:case(>=, 2, extra_ratnum, 'XLOG 6.5.6, XLOG 1') :-
   5#2 >= 10#7.
runner:case(>=, 2, extra_ratnum, 'XLOG 6.5.6, XLOG 2') :-
   \+ -5#2 >= 3.
runner:case(>=, 2, extra_ratnum, 'XLOG 6.5.6, XLOG 3') :-
   4 >= 10#7.
runner:case(>=, 2, extra_ratnum, 'XLOG 6.5.6, XLOG 4') :-
   12 >= -6.
runner:case(>=, 2, extra_ratnum, 'XLOG 6.5.6, XLOG 5') :-
   2#3 >= 2#3.
runner:case(>=, 2, extra_ratnum, 'XLOG 6.5.6, XLOG 6') :-
   -10 >= -10.

/* min(X, Y) */

runner:ref(min, -3, extra_ratnum, 'XLOG 6.5.7').
runner:case(min, -3, extra_ratnum, 'XLOG 6.5.7, XLOG 1') :-
   X is min(5#2, 10#7), X == 10#7.
runner:case(min, -3, extra_ratnum, 'XLOG 6.5.7, XLOG 2') :-
   X is min(-5#2, 3), X == -5#2.
runner:case(min, -3, extra_ratnum, 'XLOG 6.5.7, XLOG 3') :-
   X is min(4, 10#7), X == 10#7.
runner:case(min, -3, extra_ratnum, 'XLOG 6.5.7, XLOG 4') :-
   X is min(12, -6), X == -6.

/* max(X, Y) */

runner:ref(max, -3, extra_ratnum, 'XLOG 6.5.8').
runner:case(max, -3, extra_ratnum, 'XLOG 6.5.8, XLOG 1') :-
   X is max(5#2, 10#7), X == 5#2.
runner:case(max, -3, extra_ratnum, 'XLOG 6.5.8, XLOG 2') :-
   X is max(-5#2, 3), X == 3.
runner:case(max, -3, extra_ratnum, 'XLOG 6.5.8, XLOG 3') :-
   X is max(4, 10#7), X == 4.
runner:case(max, -3, extra_ratnum, 'XLOG 6.5.8, XLOG 4') :-
   X is max(12, -6), X == 12.
