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

:- use_module(library(misc/bits)).

% bitcount/2
runner:ref(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1').
runner:case(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1, XLOG 1') :-
   X is bitcount(12),
   X == 2.
runner:case(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1, XLOG 2') :-
   X is bitcount(-12),
   X == 3.
runner:case(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1, XLOG 3') :-
   X is bitcount(11*2^100),
   X == 3.
runner:case(bitcount, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.1, XLOG 4') :-
   X is bitcount(-12*2^100),
   X == 103.

% bitlength/2
runner:ref(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2').
runner:case(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2, XLOG 1') :-
   X is bitlength(12),
   X == 4.
runner:case(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2, XLOG 2') :-
   X is bitlength(-12),
   X == 4.
runner:case(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2, XLOG 3') :-
   X is bitlength(12*2^100),
   X == 104.
runner:case(bitlength, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.2, XLOG 4') :-
   X is bitlength(-12*2^100),
   X == 104.

% lowestsetbit/2
runner:ref(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3').
runner:case(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3, XLOG 1') :-
   X is lowestsetbit(12),
   X == 2.
runner:case(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3, XLOG 2') :-
   X is lowestsetbit(-12),
   X == 2.
runner:case(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3, XLOG 3') :-
   X is lowestsetbit(12*2^100),
   X == 102.
runner:case(lowestsetbit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.3, XLOG 4') :-
   X is lowestsetbit(-12*2^100),
   X == 102.

% setbit/3
runner:ref(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4').
runner:case(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4, XLOG 1') :-
   X is setbit(4,77),
   X == 93.
runner:case(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4, XLOG 2') :-
   X is setbit(3,-77),
   X == -69.
runner:case(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4, XLOG 3') :-
   X is setbit(104,77*2^100),
   X =:= 93*2^100.
runner:case(setbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.4, XLOG 4') :-
   X is setbit(103,-77*2^100),
   X =:= -69*2^100.

% clearbit/3
runner:ref(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5').
runner:case(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5, XLOG 1') :-
   X is clearbit(3,77),
   X == 69.
runner:case(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5, XLOG 2') :-
   X is clearbit(4,-77),
   X == -93.
runner:case(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5, XLOG 3') :-
   X is clearbit(103,77*2^100),
   X =:= 69*2^100.
runner:case(clearbit, 3, misc_bitprop, 'CLP(FD) 0.9.2, 1.5, XLOG 4') :-
   X is clearbit(104,-77*2^100),
   X =:= -93*2^100.

% sys_test_bit/2
runner:ref(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.6').
runner:case(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.6, XLOG 1') :-
   sys_test_bit(3, 77).
runner:case(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.6, XLOG 2') :-
   \+ sys_test_bit(4, 77).
runner:case(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.6, XLOG 3') :-
   X is -77*2^100,
   sys_test_bit(104, X).
runner:case(sys_test_bit, 2, misc_bitprop, 'CLP(FD) 0.9.2, 1.6, XLOG 4') :-
   X is -77*2^100,
   \+ sys_test_bit(103, X).
