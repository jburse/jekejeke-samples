/**
 * Prolog test cases for the symbolic same and before.
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

:- use_module(library(groebner/generic)).
:- use_module(library(misc/residue)).
:- use_module(library(basic/random)).

% simp_quorem/4
runner:ref(simp_quorem, 4, gauss_utilpoly, 'gauss 0.9.2, 4.1').
runner:case(simp_quorem, 4, gauss_utilpoly, 'gauss 0.9.2, 4.1, XLOG 1') :-
   quorem(X^3-2, X-1, Q, R), printable(Q, P), printable(R, S),
   P == 1+X+X^2, S == - 1.
runner:case(simp_quorem, 4, gauss_utilpoly, 'gauss 0.9.2, 4.1, XLOG 2') :-
   quorem(3*X^3-2, X-1, Q, R), printable(Q, P), printable(R, S),
   P == 3+3*X+3*X^2, S == 1.
runner:case(simp_quorem, 4, gauss_utilpoly, 'gauss 0.9.2, 4.1, XLOG 3') :-
   quorem(X^3-Y^3, X-Y, Q, R), printable(Q, P), printable(R, S),
   P == X^2+X*Y+Y^2, S == 0.
runner:case(simp_quorem, 4, gauss_utilpoly, 'gauss 0.9.2, 4.1, XLOG 4') :-
   quorem(sqrt(20)*X^3-sqrt(45)*Y^3, X-Y, Q, R), printable(Q, P), printable(R, S),
   P == sqrt(45)*X^2+sqrt(45)*X*Y+sqrt(45)*Y^2, S == -sqrt(5)*X^3.
runner:case(simp_quorem, 4, gauss_utilpoly, 'gauss 0.9.2, 4.1, XLOG 5') :-
   catch(quorem(_/_, _, _, _), error(E, _), true),
   E == existence_error(procedure, fraction:gen_div/4).

% simp_reduced/3
runner:ref(simp_reduced, 3, gauss_utilpoly, 'gauss 0.9.2, 4.2').
runner:case(simp_reduced, 3, gauss_utilpoly, 'gauss 0.9.2, 4.2, XLOG 1') :-
   reduced(2*A^3-4, R, F), printable(R, S), printable(F, G),
   S == 2-A^3, G == - 2.
runner:case(simp_reduced, 3, gauss_utilpoly, 'gauss 0.9.2, 4.2, XLOG 2') :-
   reduced(1+(1-sqrt(2))*A^2, R, F), printable(R, S), printable(F, G),
   S == 1+sqrt(2)-A^2, G == - 1+sqrt(2).
runner:case(simp_reduced, 3, gauss_utilpoly, 'gauss 0.9.2, 4.2, XLOG 3') :-
   catch(reduced(_/_, _, _), error(E, _), true),
   E == existence_error(procedure, fraction:gen_red/3).

% eval_hipow/3
runner:ref(eval_hipow, 3, gauss_utilpoly, 'gauss 0.9.3, 4.3').
runner:case(eval_hipow, 3, gauss_utilpoly, 'gauss 0.9.3, 4.3, XLOG 1') :-
   X is hipow(-77/33, _), X == 0.
runner:case(eval_hipow, 3, gauss_utilpoly, 'gauss 0.9.3, 4.3, XLOG 2') :-
   X is hipow(A*(A+1)-A^2, A), X == 1.
runner:case(eval_hipow, 3, gauss_utilpoly, 'gauss 0.9.3, 4.3, XLOG 3') :-
   X is hipow((A+B*A)*(A^2-B), A), X == 3.
runner:case(eval_hipow, 3, gauss_utilpoly, 'gauss 0.9.3, 4.3, XLOG 4') :-
   catch(_ is hipow(1/_, _), error(E, _), true),
   E == existence_error(procedure, fraction:hipow/3).

% eval_degree/2
runner:ref(eval_degree, 2, gauss_utilpoly, 'gauss 0.9.2, 4.4').
runner:case(eval_degree, 2, gauss_utilpoly, 'gauss 0.9.2, 4.3, XLOG 1') :-
   X is degree(-77/33), X == 0.
runner:case(eval_degree, 2, gauss_utilpoly, 'gauss 0.9.2, 4.4, XLOG 2') :-
   X is degree(A*(A+1)-A^2), X == 1.
runner:case(eval_degree, 2, gauss_utilpoly, 'gauss 0.9.2, 4.4, XLOG 3') :-
   X is degree((A+B*A)*(A^2-B)), X == 4.
runner:case(eval_degree, 2, gauss_utilpoly, 'gauss 0.9.2, 4.4, XLOG 4') :-
   catch(_ is degree(1/_), error(E, _), true),
   E == existence_error(procedure, fraction:degree/2).

% eval_randpoly/2
runner:ref(eval_randpoly, 2, gauss_utilpoly, 'gauss 0.9.2, 4.5').
runner:case(eval_randpoly, 2, gauss_utilpoly, 'gauss 0.9.2, 4.5, XLOG 1') :-
   random_new(123, R), set_prolog_flag(sys_random, R), X is randpoly([A]),
   printable(X, Y), Y == -A-2*A^2.
runner:case(eval_randpoly, 2, gauss_utilpoly, 'gauss 0.9.2, 4.5, XLOG 2') :-
   random_new(456, R), set_prolog_flag(sys_random, R), X is randpoly([A, B]),
   printable(X, Y), Y == - 5+(2-5*A)*B+A*B^2.
runner:case(eval_randpoly, 2, gauss_utilpoly, 'gauss 0.9.2, 4.5, XLOG 3') :-
   catch(_ is randpoly(5), error(E, _), true),
   E == existence_error(procedure, integer:randpoly/2).
