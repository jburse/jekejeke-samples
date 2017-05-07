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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

% bitcount/2
runner:ref(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1').
runner:case(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1, XLOG 1') :- true.

% bitlength/2
runner:ref(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2').
runner:case(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2, XLOG 1') :- true.

% lowestsetbit/2
runner:ref(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3').
runner:case(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3, XLOG 1') :- true.

% setbit/3
runner:ref(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4').
runner:case(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4, XLOG 1') :- true.

% clearbit/3
runner:ref(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5').
runner:case(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5, XLOG 1') :- true.

% sys_test_bit/2
runner:ref(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.6').
runner:case(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.56, XLOG 1') :- true.
