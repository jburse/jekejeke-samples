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
:- use_module(library(misc/residue)).

% eval_integer/2
runner:ref(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 1.1').
runner:case(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 1.1, XLOG 1') :-
   X is integer(9/5),
   X == 1.
runner:case(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 1.1, XLOG 2') :-
   X is integer(-9/5),
   X == -1.
runner:case(eval_integer, 2, groebner_nonfrac, 'groebner 0.9.2, 1.1, XLOG 3') :-
   catch(_ is ceiling(2*_), error(E,_), true),
   E = type_error(value,_).

% eval_floor/2
runner:ref(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 1.2').
runner:case(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 1.2, XLOG 1') :-
   X is floor(-100),
   X == -100.
runner:case(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 1.2, XLOG 2') :-
   X is floor(9/5),
   X == 1.
runner:case(eval_floor, 2, groebner_nonfrac, 'groebner 0.9.2, 1.2, XLOG 3') :-
   X is floor(-9/5),
   X == -2.

% eval_ceiling/2
runner:ref(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3').
runner:case(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, XLOG 1') :-
   X is ceiling(9/5),
   X == 2.
runner:case(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, XLOG 2') :-
   X is ceiling(-9/5),
   X == -1.
runner:case(eval_ceiling, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, XLOG 3') :-
   catch(_ is ceiling(_), error(E,_), true),
   E = type_error(number,_).

% simp_quorem/4
runner:ref(simp_quorem, 4, groebner_nonfrac, 'groebner 0.9.2, 1.4').
runner:case(simp_quorem, 4, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 1') :-
   A is X^3-2,
   B is X-1,
   quorem(A, B, Q, R),
   printable(Q, P),
   printable(R, S),
   P == 1+X+X^2,
   S == -1.
runner:case(simp_quorem, 4, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 2') :-
   A is 3*X^3-2,
   B is X-1,
   quorem(A, B, Q, R),
   printable(Q, P),
   printable(R, S),
   P == 3+3*X+3*X^2,
   S == 1.
runner:case(simp_quorem, 4, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 3') :-
   quorem(X^3-Y^3, X-Y, Q, R),
   printable(Q, P),
   printable(R, S),
   P == X^2+X*Y+Y^2,
   S == 0.
runner:case(simp_quorem, 4, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 4') :-
   quorem(sqrt(20)*X^3-sqrt(45)*Y^3, X-Y, Q, R),
   printable(Q, P),
   printable(R, S),
   P == sqrt(45)*X^2+sqrt(45)*X*Y+sqrt(45)*Y^2,
   S == -sqrt(5)*X^3.
runner:case(simp_quorem, 4, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 5') :-
   catch(quorem(_/_, _, _, _), error(E,_), true),
   E == existence_error(procedure,fraction:gen_div/4).

% simp_reduced/3
runner:ref(simp_reduced, 3, groebner_nonfrac, 'groebner 0.9.2, 1.5').
runner:case(simp_reduced, 3, groebner_nonfrac, 'groebner 0.9.2, 1.5, XLOG 1') :-
   reduced(2*A^3-4, R, F),
   printable(R, S),
   printable(F, G),
   S == 2-A^3,
   G == -2.
runner:case(simp_reduced, 3, groebner_nonfrac, 'groebner 0.9.2, 1.5, XLOG 2') :-
   reduced(1+(1-sqrt(2))*A^2, R, F),
   printable(R, S),
   printable(F, G),
   S == 1+sqrt(2)-A^2,
   G == -1+sqrt(2).
runner:case(simp_reduced, 3, groebner_nonfrac, 'groebner 0.9.2, 1.5, XLOG 3') :-
   catch(reduced(_/_, _, _), error(E,_), true),
   E == existence_error(procedure,fraction:gen_red/3).
