/**
 * Prolog code for the multi precision special functions.
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

:- use_module(library(decimal/multi)).

% mp_sin/3.
runner:ref(mp_sin, 3, decimal_expsin, 'decimal 0.9.1, 3.1').
runner:case(mp_sin, 3, decimal_expsin, 'decimal 0.9.1, 3.1, XLOG 1') :-
   X is mp(sin(0),16),
   X == 0d0.
runner:case(mp_sin, 3, decimal_expsin, 'decimal 0.9.1, 3.1, XLOG 2') :-
   X is mp(sin(-pi/6),16),
   X == -0d0.4999999999999997.
runner:case(mp_sin, 3, decimal_expsin, 'decimal 0.9.1, 3.1, XLOG 3') :-
   X is mp(sin(pi/6),16),
   X == 0d0.4999999999999997.
runner:case(mp_sin, 3, decimal_expsin, 'decimal 0.9.1, 3.1, XLOG 4') :-
   X is mp(sin(pi/12),16),
   X == 0d0.2588190451025209.
runner:case(mp_sin, 3, decimal_expsin, 'decimal 0.9.1, 3.1, XLOG 5') :-
   X is mp(sin(17*pi/12),16),
   X == -0d0.9659258262890681.

% mp_cos/3.
runner:ref(mp_cos, 3, decimal_expsin, 'decimal 0.9.1, 3.2').
runner:case(mp_cos, 3, decimal_expsin, 'decimal 0.9.1, 3.2, XLOG 1') :-
   X is mp(cos(0),16),
   X == 0d1.
runner:case(mp_cos, 3, decimal_expsin, 'decimal 0.9.1, 3.2, XLOG 2') :-
   X is mp(cos(-pi/6),16),
   X == 0d0.8660254037844383.
runner:case(mp_cos, 3, decimal_expsin, 'decimal 0.9.1, 3.2, XLOG 3') :-
   X is mp(cos(pi/6),16),
   X == 0d0.8660254037844383.
runner:case(mp_cos, 3, decimal_expsin, 'decimal 0.9.1, 3.2, XLOG 4') :-
   X is mp(cos(pi/12),16),
   X == 0d0.9659258262890682.
runner:case(mp_cos, 3, decimal_expsin, 'decimal 0.9.1, 3.2, XLOG 5') :-
   X is mp(sin(17*pi/12),16),
   X == -0d0.9659258262890681.

% mp_tan/3.
runner:ref(mp_tan, 3, decimal_expsin, 'decimal 0.9.1, 3.3').
runner:case(mp_tan, 3, decimal_expsin, 'decimal 0.9.1, 3.3, XLOG 1') :-
   X is mp(tan(5*pi/6),16),
   X == 0d0.5773502691896238.
runner:case(mp_tan, 3, decimal_expsin, 'decimal 0.9.1, 3.3, XLOG 2') :-
   X is mp(tan(11*pi/12),16),
   X == -0d0.2679491924311233.
runner:case(mp_tan, 3, decimal_expsin, 'decimal 0.9.1, 3.3, XLOG 3') :-
   X is mp(tan(11*pi/6),16),
   X == -0d0.5773502691896277.
runner:case(mp_tan, 3, decimal_expsin, 'decimal 0.9.1, 3.3, XLOG 4') :-
   X is mp(tan(23*pi/12),16),
   X == -0d0.2679491924311233.

% mp_atan/3.
runner:ref(mp_atan, 3, decimal_expsin, 'decimal 0.9.1, 3.4').
runner:case(mp_atan, 3, decimal_expsin, 'decimal 0.9.1, 3.4, XLOG 1') :-
   X is mp(atan(0),16),
   X == 0d0.
runner:case(mp_atan, 3, decimal_expsin, 'decimal 0.9.1, 3.4, XLOG 2') :-
   X is mp(atan(-1),16),
   X == -0d0.7853981633974488.
runner:case(mp_atan, 3, decimal_expsin, 'decimal 0.9.1, 3.4, XLOG 3') :-
   X is mp(atan(3/4),16),
   X == 0d0.6435011087932848.
runner:case(mp_atan, 3, decimal_expsin, 'decimal 0.9.1, 3.4, XLOG 3') :-
   X is mp(atan(1/3),16),
   X == 0d0.3217505543966422.

% mp_exp/3.
runner:ref(mp_exp, 3, decimal_expsin, 'decimal 0.9.1, 3.5').
runner:case(mp_exp, 3, decimal_expsin, 'decimal 0.9.1, 3.5, XLOG 1') :-
   X is mp(exp(0),16),
   X == 0d1.
runner:case(mp_exp, 3, decimal_expsin, 'decimal 0.9.1, 3.5, XLOG 2') :-
   X is mp(exp(-5/2),16),
   X == 0d0.08208499862389882.
runner:case(mp_exp, 3, decimal_expsin, 'decimal 0.9.1, 3.5, XLOG 3') :-
   X is mp(exp(15/2),16),
   X == 0d1808.042414456062.
runner:case(mp_exp, 3, decimal_expsin, 'decimal 0.9.1, 3.5, XLOG 4') :-
   X is mp(exp(3/4),16),
   X == 0d2.117000016612674.
runner:case(mp_exp, 3, decimal_expsin, 'decimal 0.9.1, 3.5, XLOG 5') :-
   X is mp(exp(1/3),16),
   X == 0d1.395612425086090.

% mp_log/3.
runner:ref(mp_log, 3, decimal_expsin, 'decimal 0.9.1, 3.6').
runner:case(mp_log, 3, decimal_expsin, 'decimal 0.9.1, 3.6, XLOG 1') :-
   catch(_ is mp(log(0),16), error(E,_), true),
   E == evaluation_error(float_underflow).
runner:case(mp_log, 3, decimal_expsin, 'decimal 0.9.1, 3.6, XLOG 2') :-
   catch(_ is mp(log(-1),16), error(E,_), true),
   E == evaluation_error(undefined).
runner:case(mp_log, 3, decimal_expsin, 'decimal 0.9.1, 3.6, XLOG 3') :-
   X is mp(log(15/2),16),
   X == 0d2.014903020542265.
runner:case(mp_log, 3, decimal_expsin, 'decimal 0.9.1, 3.6, XLOG 4') :-
   X is mp(log(1/3),16),
   X == -0d1.098612288668110.

% mp_pow/4.
runner:ref(mp_pow, 4, decimal_expsin, 'decimal 0.9.1, 3.7').
runner:case(mp_pow, 4, decimal_expsin, 'decimal 0.9.1, 3.7, XLOG 1') :-
   X is mp((1/10)**0,16),
   X == 0d1.
runner:case(mp_pow, 4, decimal_expsin, 'decimal 0.9.1, 3.7, XLOG 2') :-
   X is mp((1/10)**2,16),
   X == 0d0.01000000000000000.
runner:case(mp_pow, 4, decimal_expsin, 'decimal 0.9.1, 3.7, XLOG 3') :-
   X is mp((1/10)** -1,16),
   X == 0d9.999999999999999.

% mp_atan2/4.
runner:ref(mp_atan2, 4, decimal_expsin, 'decimal 0.9.1, 3.8').
runner:case(mp_atan2, 4, decimal_expsin, 'decimal 0.9.1, 3.8, XLOG 1') :-
   X is mp(atan2(-1,-1),16),
   X == -0d2.356194490192346.
runner:case(mp_atan2, 4, decimal_expsin, 'decimal 0.9.1, 3.8, XLOG 2') :-
   X is mp(atan2(1,-1),16),
   X == 0d2.356194490192346.
runner:case(mp_atan2, 4, decimal_expsin, 'decimal 0.9.1, 3.8, XLOG 3') :-
   X is mp(atan2(-1,1),16),
   X == -0d0.7853981633974488.
runner:case(mp_atan2, 4, decimal_expsin, 'decimal 0.9.1, 3.8, XLOG 4') :-
   X is mp(atan2(1,1),16),
   X == 0d0.7853981633974488.

% mp_asin/3.
runner:ref(mp_asin, 3, decimal_expsin, 'decimal 0.9.1, 3.9').
runner:case(mp_asin, 34, decimal_expsin, 'decimal 0.9.1, 3.9, XLOG 1') :-
   X is mp(asin(0),16),
   X == 0d0.
runner:case(mp_asin, 34, decimal_expsin, 'decimal 0.9.1, 3.9, XLOG 2') :-
   X is mp(asin(1),16),
   X == 0d1.570796326794898.
runner:case(mp_asin, 34, decimal_expsin, 'decimal 0.9.1, 3.9, XLOG 3') :-
   X is mp(asin(-1/2),16),
   X == -0d0.5235987755982997.
runner:case(mp_asin, 34, decimal_expsin, 'decimal 0.9.1, 3.9, XLOG 4') :-
   X is mp(asin(1/3),16),
   X == 0d0.3398369094541220.

% mp_acos/3.
runner:ref(mp_acos, 3, decimal_expsin, 'decimal 0.9.1, 3.10').
runner:case(mp_acos, 3, decimal_expsin, 'decimal 0.9.1, 3.10, XLOG 1') :-
   X is mp(acos(0),16),
   X == 0d1.570796326794898.
runner:case(mp_acos, 3, decimal_expsin, 'decimal 0.9.1, 3.10, XLOG 2') :-
   X is mp(acos(1),16),
   X == 0d0.
runner:case(mp_acos, 3, decimal_expsin, 'decimal 0.9.1, 3.10, XLOG 3') :-
   X is mp(acos(-1/2),16),
   X == 0d2.094395102393197.
runner:case(mp_acos, 3, decimal_expsin, 'decimal 0.9.1, 3.10, XLOG 4') :-
   X is mp(acos(1/3),16),
   X == 0d1.230959417340776.
