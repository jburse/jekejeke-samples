/**
 * Prolog code for the extra structure theory test cases.
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

% mp_sqrt/3
runner:ref(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.1').
runner:case(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.1, XLOG 1') :-
   X is mp(sqrt(1000),16),
   X =:= 0d31.62277660168384.
runner:case(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.1, XLOG 2') :-
   X is mp(sqrt(0),16),
   X =:= 0.
runner:case(mp_sqrt, 3, decimal_addmul, 'decimal 0.9.1, 2.1, XLOG 3') :-
   catch(_ is mp(sqrt(-1),16), error(E,_), true),
   E == evaluation_error(undefined).

% mp_abnormal/1.
runner:ref(mp_abnormal, 1, decimal_addmul, 'decimal 0.9.1, 2.2').
runner:case(mp_abnormal, 1, decimal_addmul, 'decimal 0.9.1, 2.2, XLOG 1') :-
   X is mp(- (2/3),16),
   X =:= -0d0.6666666666666667.
