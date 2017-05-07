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

% ulp/2
runner:ref(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.1').
runner:case(ulp, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.1, XLOG 1') :- true.

% gcd/3
runner:ref(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2').
runner:case(gcd, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.2, XLOG 1') :- true.

% lcm/3
runner:ref(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3').
runner:case(lcm, 3, misc_numtheo, 'CLP(FD) 0.9.2, 1.3, XLOG 1') :- true.

% isqrt/2
runner:ref(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.4').
runner:case(isqrt, 2, misc_numtheo, 'CLP(FD) 0.9.2, 1.4, XLOG 1') :- true.
