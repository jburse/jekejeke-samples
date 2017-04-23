/**
 * Prolog code for the multi precision helper and dispatcher.
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

:- use_module(library(decimal/helper)).
:- use_module(library(decimal/scale)).
:- use_module(library(decimal/multi)).

% dec_log10/2
runner:ref(dec_log10, 2, decimal_precision, 'decimal 0.9.1, 1.1').
runner:case(dec_log10, 2, decimal_precision, 'decimal 0.9.1, 1.1, XLOG 1') :-
   dec_log10(0d1234.0, X),
   X == -3.0913151596972224.
runner:case(dec_log10, 2, decimal_precision, 'decimal 0.9.1, 1.1, XLOG 2') :-
   dec_log10(0d1234.0E100, X),
   X == -103.09131515969722.

% dec_decomp/3
runner:ref(dec_decomp, 3, decimal_precision, 'decimal 0.9.1, 1.2').
runner:case(dec_decomp, 3, decimal_precision, 'decimal 0.9.1, 1.2, XLOG 1') :-
   dec_decomp(0d1234.0E100, E, M),
   E == 103,
   M == 0d1.2340.
runner:case(dec_decomp, 3, decimal_precision, 'decimal 0.9.1, 1.2, XLOG 2') :-
   dec_decomp(0d1.2340, E, M),
   E == 0,
   M == 0d1.2340.

% bin_decomp/4
runner:ref(bin_decomp, 4, decimal_precision, 'decimal 0.9.1, 1.3').
runner:case(bin_decomp, 4, decimal_precision, 'decimal 0.9.1, 1.3, XLOG 1') :-
   C is new_context(16),
   bin_decomp(0d1.2340, C, E, M),
   E == 0,
   M == 0d1.2340.
runner:case(bin_decomp, 4, decimal_precision, 'decimal 0.9.1, 1.3, XLOG 2') :-
   C is new_context(16),
   bin_decomp(0d5.6780, C, E, M),
   E == 2,
   M == 0d1.4195.

% mp_abnormal/1.
runner:ref(mp_abnormal, 1, decimal_precision, 'decimal 0.9.1, 1.4').
runner:case(mp_abnormal, 1, decimal_precision, 'decimal 0.9.1, 1.4, XLOG 1') :-
   catch(_ is mp(_,16), error(E,_), true),
   E == instantiation_error.
runner:case(mp_abnormal, 1, decimal_precision, 'decimal 0.9.1, 1.4, XLOG 2') :-
   X is mp(- (2/3),16),
   X == -0d0.6666666666666667.

% mp_decimal/2.
runner:ref(mp_decimal, 2, decimal_precision, 'decimal 0.9.1, 1.5').
runner:case(mp_decimal, 2, decimal_precision, 'decimal 0.9.1, 1.5, XLOG 1') :-
   X is mp(decimal(3),16),
   X == 0d3.
runner:case(mp_decimal, 2, decimal_precision, 'decimal 0.9.1, 1.5, XLOG 2') :-
   X is mp(decimal(3.141592653589793),16),
   X == 0d3.141592653589793.
