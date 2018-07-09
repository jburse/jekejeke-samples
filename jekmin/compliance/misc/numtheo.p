/**
 * Prolog code for the number theory test casess.
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

:- use_module(library(misc/elem)).

% ulp/2
runner:ref(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.1').
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.1, XLOG 1') :-
   X is ulp(12),
   X == 1.
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.1, XLOG 2') :-
   X is ulp(12.0),
   X == 1.7763568394002505E-15.
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.1, XLOG 3') :-
   X is ulp(0d12.000),
   X = 0d0.001.

% gcd/3
runner:ref(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2').
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 1') :-
   X is gcd(12,18),
   X == 6.
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 2') :-
   X is gcd(-12,18),
   X == 6.
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 3') :-
   X is gcd(12*2^100,-18*2^100),
   X =:= 6*2^100.
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 4') :-
   X is gcd(-12*2^100,-18*2^100),
   X =:= 6*2^100.
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 5') :-
   X is gcd(3,0),
   X == 3.
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 6') :-
   X is gcd(0,3*2^100),
   X =:= 3*2^100.

% lcm/3
runner:ref(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3').
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 1') :-
   X is lcm(12,18),
   X == 36.
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 2') :-
   X is lcm(12,-18),
   X == -36.
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 3') :-
   X is lcm(-12*2^100,18*2^100),
   X =:= -36*2^100.
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 4') :-
   X is lcm(-12*2^100,-18*2^100),
   X =:= 36*2^100.
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 5') :-
   X is lcm(0,3),
   X == 0.
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 6') :-
   X is lcm(3*2^100,0),
   X == 0.

% isqrt/2
runner:ref(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.4').
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.4, XLOG 1') :-
   X is isqrt(77),
   X == 8.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.4, XLOG 2') :-
   X is isqrt(77*2^100),
   X == 9879731586312133.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.4, XLOG 3') :-
   catch(_ is isqrt(-33), error(E,_), true),
   E == evaluation_error(undefined).

% sqrtrem/3
runner:ref(sqrtrem, 3, misc_numtheo, 'CLP(FD) 1.0.0, 1.5').
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 1.0.0, 1.5, XLOG 1') :-
   sqrtrem(77, X, Y),
   X == 8,
   Y == 13.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 1.0.0, 1.5, XLOG 2') :-
   Z is 77*2^100,
   sqrtrem(Z, X, Y),
   X == 9879731586312133,
   Y == 8000882843804263.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 1.0.0, 1.5, XLOG 3') :-
   catch(sqrtrem(-33, _, _), error(E,_), true),
   E == evaluation_error(undefined).
