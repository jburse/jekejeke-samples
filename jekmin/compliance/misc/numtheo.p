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

:- use_module(library(experiment/elem)).

% ulp(X, Y)

runner:ref(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.1').
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.1, XLOG 1') :-
   X is ulp(12), X == 1.
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.1, XLOG 2') :-
   X is ulp(12.0), X == 1.7763568394002505E-15.
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.1, XLOG 3') :-
   X is ulp(0d12.000), X = 0d0.001.

% isqrt(X, Y)

runner:ref(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.2').
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.2, XLOG 1') :-
   X is isqrt(77), X == 8.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.2, XLOG 2') :-
   X is isqrt(77*2^100),
   X == 9879731586312133.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.2, XLOG 3') :-
   X is isqrt(7^77),
   X == 343775197739156103851373356134328.
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.2, XLOG 4') :-
   catch(_ is isqrt(-33), error(E, _), true),
   E == evaluation_error(undefined).

% sqrtrem(X, Y, Z)

runner:ref(sqrtrem, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.3').
runner:case(sqrtrem, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.3, XLOG 1') :-
   sqrtrem(77, X, Y), X == 8, Y == 13.
runner:case(sqrtrem, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.3, XLOG 2') :-
   Z is 77*2^100, sqrtrem(Z, X, Y),
   X == 9879731586312133,
   Y == 8000882843804263.
runner:case(sqrtrem, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.3, XLOG 3') :-
   Z is 7^77, sqrtrem(Z, X, Y),
   X == 343775197739156103851373356134328,
   Y == 197147407899293772627110707027623.
runner:case(sqrtrem, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.3, XLOG 4') :-
   catch(sqrtrem(-33, _, _), error(E, _), true),
   E == evaluation_error(undefined).

% iroot(X, Y, Z)

runner:ref(iroot, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.4').
runner:case(iroot, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.4, XLOG 1') :-
   X is iroot(77, 5), X == 2.
runner:case(iroot, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.4, XLOG 2') :-
   X is iroot(77*2^100, 5),
   X == 2499758.
runner:case(iroot, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.4, XLOG 3') :-
   X is iroot(7^116, 3),
   X == 475471197459215430348196293525957.
runner:case(iroot, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.4, XLOG 4') :-
   catch(_ is iroot(-33, 5), error(E, _), true),
   E == evaluation_error(undefined).
runner:case(iroot, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.4, XLOG 5') :-
   catch(_ is iroot(33, -5), error(E, _), true),
   E == evaluation_error(undefined).

% rootrem(X, Y, Z, T)

runner:ref(rootrem, 4, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.5').
runner:case(rootrem, 4, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.5, XLOG 1') :-
   rootrem(77, 5, X, Y), X == 2, Y == 45.
runner:case(rootrem, 4, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.5, XLOG 2') :-
   Z is 77*2^100, rootrem(Z, 5, X, Y),
   X == 2499758,
   Y == 102692834401544299944401184.
runner:case(rootrem, 4, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.5, XLOG 3') :-
   Z is 7^116, rootrem(Z, 3, X, Y),
   X == 475471197459215430348196293525957,
   Y == 56535314166992499802150711925892543414683836939559609611849987108.
runner:case(rootrem, 4, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.5, XLOG 4') :-
   catch(rootrem(-33, 5, _, _), error(E, _), true),
   E == evaluation_error(undefined).
runner:case(rootrem, 4, misc_numtheo, 'CLP(FD) 0.9.2, 1.2.5, XLOG 5') :-
   catch(rootrem(33, -5, _, _), error(E, _), true),
   E == evaluation_error(undefined).
