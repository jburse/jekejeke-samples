/**
 * Prolog test cases for the symbolic series expansion.
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

% series_taylor/4
runner:ref(series_taylor, 4, leibniz_expansion, 'leibniz 0.9.1, 2.1').
runner:case(series_taylor, 4, leibniz_expansion, 'leibniz 0.9.1, 2.1, XLOG 1') :-
   X is taylor(1/(1+A), A, 5), printable(X, Y),
   Y == 1-A+A^2-A^3+A^4-A^5.
runner:case(series_taylor, 4, leibniz_expansion, 'leibniz 0.9.1, 2.1, XLOG 2') :-
   X is taylor(1/(1+B*A), A, 5), printable(X, Y),
   Y == 1-B*A+B^2*A^2-B^3*A^3+B^4*A^4-B^5*A^5.

% series_taylor/5
runner:ref(series_taylor, 5, leibniz_expansion, 'leibniz 0.9.1, 2.2').
runner:case(series_taylor, 5, leibniz_expansion, 'leibniz 0.9.1, 2.2, XLOG 1') :-
   X is taylor(1/A, A, 5, 1), printable(X, Y),
   Y == 6-15*A+20*A^2-15*A^3+6*A^4-A^5.
runner:case(series_taylor, 5, leibniz_expansion, 'leibniz 0.9.1, 2.2, XLOG 2') :-
   X is taylor(1/(B*A), A, 5, 1), printable(X, Y),
   Y == (6-15*A+20*A^2-15*A^3+6*A^4-A^5)/B.

% series_laurent/4
runner:ref(series_laurent, 4, leibniz_expansion, 'leibniz 0.9.1, 2.3').
runner:case(series_laurent, 4, leibniz_expansion, 'leibniz 0.9.1, 2.3, XLOG 1') :-
   X is laurent(1/(1+A), A, 5), printable(X, Y),
   Y == (1-A+A^2-A^3+A^4)/A^5.
runner:case(series_laurent, 4, leibniz_expansion, 'leibniz 0.9.1, 2.3, XLOG 2') :-
   X is laurent(B/(1+A), A, 5), printable(X, Y),
   Y == (4/5*B-3/4*B*A+2/3*B*A^2-1/2*B*A^3+B*A^4)/A^5.

% series_laurent/5
runner:ref(series_laurent, 5, leibniz_expansion, 'leibniz 0.9.1, 2.4').
runner:case(series_laurent, 5, leibniz_expansion, 'leibniz 0.9.1, 2.4, XLOG 1') :-
   X is laurent(1/A, A, 5, 1), printable(X, Y), Y == 1/A.
runner:case(series_laurent, 5, leibniz_expansion, 'leibniz 0.9.1, 2.4, XLOG 2') :-
   X is laurent(1/(B*A), A, 5, 1), printable(X, Y),
   Y == (1/30-7/24*A+(1+1/6)*A^2-(2+7/12)*A^3+(2+2/3)*A^4+1/120*A^5)/(B*A^5).
