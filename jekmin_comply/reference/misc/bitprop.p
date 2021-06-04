/**
 * Prolog code for the bit properties test casess.
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

:- use_module(library(experiment/bits)).
:- use_module(library(experiment/elem)).

% setbit(X, Y)

runner:ref(setbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.1').
runner:case(setbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.1, XLOG 1') :-
   X is setbit(77, 4), X == 93.
runner:case(setbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.1, XLOG 2') :-
   X is setbit(-77, 3), X == -69.
runner:case(setbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.1, XLOG 3') :-
   X is setbit(77*2^100, 104), X =:= 93*2^100.
runner:case(setbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.1, XLOG 4') :-
   X is setbit(-77*2^100, 103), X =:= -69*2^100.

% clearbit(X, Y)

runner:ref(clearbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.2').
runner:case(clearbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.2, XLOG 1') :-
   X is clearbit(77, 3), X == 69.
runner:case(clearbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.2, XLOG 2') :-
   X is clearbit(-77, 4), X == -93.
runner:case(clearbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.2, XLOG 3') :-
   X is clearbit(77*2^100, 103), X =:= 69*2^100.
runner:case(clearbit, -3, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.2, XLOG 4') :-
   X is clearbit(-77*2^100, 104), X =:= -93*2^100.

% testbit(X, Y)

runner:ref(testbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.3').
runner:case(testbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.3, XLOG 1') :-
   testbit(77, 3).
runner:case(testbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.3, XLOG 2') :-
   \+ testbit(77, 4).
runner:case(testbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.3, XLOG 3') :-
   X is -77*2^100, testbit(X, 104).
runner:case(testbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1.3, XLOG 4') :-
   X is -77*2^100, \+ testbit(X, 103).

% modinv(B, M, R):

runner:ref(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4').
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 1') :-
   X is modinv(42, 2017), X == 1969.
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 2') :-
   X is modinv(2059, 2017), X == 1969.
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 3') :-
   X is modinv(-1975, 2017), X == 1969.
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 4') :-
   X is modinv(2469396481, 4294967296), X == 1825570817.
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 5') :-
   catch(_ is modinv(42, 64), error(E, _), true),
   E == evaluation_error(not_invertible).
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 6') :-
   catch(_ is modinv(2469396480, 4294967296), error(E, _), true),
   E == evaluation_error(not_invertible).
runner:case(modinv, -4, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.4, XLOG 7') :-
   catch(_ is modinv(42, -3), error(E, _), true),
   E == evaluation_error(illegal_modulus).

% modpow(B, E, M, R):

runner:ref(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5').
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 1') :-
   X is modpow(42, 10, 2017), X == 1426.
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 2') :-
   X is modpow(2059, -1, 2017), X == 1969.
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 3') :-
   X is modpow(-1975, -1, 2017), X == 1969.
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 4') :-
   X is modpow(12367, 65536, 4294967296), X == 2469396481.
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 5') :-
   catch(_ is modpow(42, -3, 64), error(E, _), true),
   E == evaluation_error(not_invertible).
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 6') :-
   catch(_ is modpow(2469396480, -3, 4294967296), error(E, _), true),
   E == evaluation_error(not_invertible).
runner:case(modpow, -5, misc_bitprop, 'CLP(FD) 1.1.7, 1.1.5, XLOG 7') :-
   catch(_ is modpow(42, 2, -3), error(E, _), true),
   E == evaluation_error(illegal_modulus).
