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

% mat_cons/3
runner:ref(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1').
runner:case(mat_cons, 3, gauss_exchstep, 'gauss 0.9.1, 2.1, XLOG 1') :- true.

% mat_at/3
runner:ref(mat_at, 3, gauss_exchstep, 'gauss 0.9.1, 2.2').
runner:case(mat_at, 3, gauss_exchstep, 'gauss 0.9.1, 2.2, XLOG 1') :- true.

% mat_at_at/4
runner:ref(mat_at_at, 4, gauss_exchstep, 'gauss 0.9.1, 2.3').
runner:case(mat_at_at, 4, gauss_exchstep, 'gauss 0.9.1, 2.3, XLOG 1') :- true.

% mat_len/2
runner:ref(mat_len, 2, gauss_exchstep, 'gauss 0.9.1, 2.4').
runner:case(mat_len, 2, gauss_exchstep, 'gauss 0.9.1, 2.4, XLOG 1') :- true.

% mat_neg/2
runner:ref(mat_neg, 2, gauss_exchstep, 'gauss 0.9.1, 2.5').
runner:case(mat_neg, 2, gauss_exchstep, 'gauss 0.9.1, 2.5, XLOG 1') :- true.

% mat_add/3
runner:ref(mat_add, 3, gauss_exchstep, 'gauss 0.9.1, 2.6').
runner:case(mat_add, 3, gauss_exchstep, 'gauss 0.9.1, 2.6, XLOG 1') :- true.

% mat_sub/3
runner:ref(mat_sub, 3, gauss_exchstep, 'gauss 0.9.1, 2.7').
runner:case(mat_sub, 3, gauss_exchstep, 'gauss 0.9.1, 2.7, XLOG 1') :- true.

% mat_mul/3
runner:ref(mat_mul, 3, gauss_exchstep, 'gauss 0.9.1, 2.8').
runner:case(mat_mul, 3, gauss_exchstep, 'gauss 0.9.1, 2.8, XLOG 1') :- true.

% mat_slash/3
runner:ref(mat_slash, 3, gauss_exchstep, 'gauss 0.9.1, 2.9').
runner:case(mat_slash, 3, gauss_exchstep, 'gauss 0.9.1, 2.9, XLOG 1') :- true.

% mat_int_pow/3
runner:ref(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10').
runner:case(mat_int_pow, 3, gauss_exchstep, 'gauss 0.9.1, 2.10, XLOG 1') :- true.
