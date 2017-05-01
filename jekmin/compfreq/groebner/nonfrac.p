/**
 * Prolog test cases for the symbolic non fraction.
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

:- use_module(library(groebner/generic)).

% eval_integer/2
runner:ref(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 4.1').
runner:case(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 4.1, XLOG 1') :-
   X is integer(9/5),
   X == 1.
runner:case(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 4.1, XLOG 2') :-
   X is integer(-9/5),
   X == -1.
runner:case(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 4.1, XLOG 3') :-
   catch(_ is ceiling(2*_), error(E,_), true),
   E = type_error(value,_).

% eval_floor/2
runner:ref(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 4.2').
runner:case(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 4.2, XLOG 1') :-
   X is floor(-100),
   X == -100.
runner:case(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 4.2, XLOG 2') :-
   X is floor(9/5),
   X == 1.
runner:case(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 4.2, XLOG 3') :-
   X is floor(-9/5),
   X == -2.

% eval_ceiling/2
runner:ref(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 4.3').
runner:case(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 4.3, XLOG 1') :-
   X is ceiling(9/5),
   X == 2.
runner:case(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 4.3, XLOG 2') :-
   X is ceiling(-9/5),
   X == -1.
runner:case(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 4.3, XLOG 3') :-
   catch(_ is ceiling(_), error(E,_), true),
   E = type_error(number,_).
