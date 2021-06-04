/**
 * Prolog code for the multi precision arithmetic functions.
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

:- use_module(library(decimal/multi)).

% mp_add/3
runner:ref(mp_add, 3, decimal_addmul, 'decimal 0.9.1, 2.1').
runner:case(mp_add, 3, decimal_addmul, 'decimal 0.9.1, 2.1, XLOG 1') :-
   X is mp(1+4/10^16, 16), X == 0d1.000000000000000.
runner:case(mp_add, 3, decimal_addmul, 'decimal 0.9.1, 2.1, XLOG 2') :-
   X is mp(1+6/10^16, 16), X == 0d1.000000000000001.
runner:case(mp_add, 3, decimal_addmul, 'decimal 0.9.1, 2.1, XLOG 3') :-
   X is mp(-0d3.162277660168380+0d3.162277660168381, 16),
   X == 0d1E-15.

% mp_sub/3
runner:ref(mp_sub, 3, decimal_addmul, 'decimal 0.9.1, 2.2').
runner:case(mp_sub, 3, decimal_addmul, 'decimal 0.9.1, 2.2, XLOG 1') :-
   X is mp(1-4/10^17, 16), X == 0d1.000000000000000.
runner:case(mp_sub, 3, decimal_addmul, 'decimal 0.9.1, 2.2, XLOG 2') :-
   X is mp(1-6/10^17, 16), X == 0d0.9999999999999999.
runner:case(mp_sub, 3, decimal_addmul, 'decimal 0.9.1, 2.2, XLOG 3') :-
   X is mp(0d3.162277660168380-0d3.162277660168381, 16),
   X == -0d1E-15.

% mp_mul/3
runner:ref(mp_mul, 3, decimal_addmul, 'decimal 0.9.1, 2.3').
runner:case(mp_mul, 3, decimal_addmul, 'decimal 0.9.1, 2.3, XLOG 1') :-
   X is mp(0d3.162277660168381*0d3.162277660168381, 16),
   X == 0d10.00000000000001.
runner:case(mp_mul, 3, decimal_addmul, 'decimal 0.9.1, 2.3, XLOG 2') :-
   X is mp(-0d3.162277660168381*0d3.162277660168380, 16),
   X == -0d10.00000000000001.
runner:case(mp_mul, 3, decimal_addmul, 'decimal 0.9.1, 2.3, XLOG 3') :-
   X is mp(-0d3.162277660168380* -0d3.162277660168380, 16),
   X == 0d10.00000000000000.
runner:case(mp_mul, 3, decimal_addmul, 'decimal 0.9.1, 2.3, XLOG 4') :-
   X is mp(0d3.162277660168379*0d3.162277660168379, 16),
   X == 0d9.999999999999998.

% mp_slash/3
runner:ref(mp_slash, 3, decimal_addmul, 'decimal 0.9.1, 2.4').
runner:case(mp_slash, 3, decimal_addmul, 'decimal 0.9.1, 2.4, XLOG 1') :-
   X is mp(10/0d3.162277660168381, 16),
   X == 0d3.162277660168378.
runner:case(mp_slash, 3, decimal_addmul, 'decimal 0.9.1, 2.4, XLOG 2') :-
   X is mp(10/ -0d3.162277660168380, 16),
   X == -0d3.162277660168379.
runner:case(mp_slash, 3, decimal_addmul, 'decimal 0.9.1, 2.4, XLOG 3') :-
   X is mp(-10/ -0d3.162277660168379, 16),
   X == 0d3.162277660168380.
runner:case(mp_slash, 3, decimal_addmul, 'decimal 0.9.1, 2.4, XLOG 4') :-
   catch(_ is mp(10/0, 16), error(E, _), true),
   E == evaluation_error(zero_divisor).

% mp_int_pow/3
runner:ref(mp_int_pow, 3, decimal_addmul, 'decimal 0.9.1, 2.5').
runner:case(mp_int_pow, 3, decimal_addmul, 'decimal 0.9.1, 2.5, XLOG 1') :-
   X is mp((1/3)^0, 16), X == 0d1.
runner:case(mp_int_pow, 3, decimal_addmul, 'decimal 0.9.1, 2.5, XLOG 2') :-
   X is mp((1/3)^ -1, 16), X == 0d3.000000000000000.
runner:case(mp_int_pow, 3, decimal_addmul, 'decimal 0.9.1, 2.5, XLOG 3') :-
   X is mp((1/3)^3, 16), X == 0d0.03703703703703703.
runner:case(mp_int_pow, 3, decimal_addmul, 'decimal 0.9.1, 2.5, XLOG 3') :-
   catch(_ is mp(0d0^ -1, 16), error(E, _), true),
   E == evaluation_error(zero_divisor).

% mp_sqrt/3
runner:ref(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.6').
runner:case(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.6, XLOG 1') :-
   X is mp(sqrt(1000), 16), X == 0d31.62277660168384.
runner:case(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.6, XLOG 2') :-
   X is mp(sqrt(0), 16), X == 0d0.
runner:case(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.6, XLOG 3') :-
   catch(_ is mp(sqrt(-1), 16), error(E, _), true),
   E == evaluation_error(undefined).

