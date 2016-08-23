/**
 * Prolog code for the symbolic derivation benchmark.
 *
 * This is the benchmark of page 222 of:
 * Warren, D.H.D. (1983): Applied Logic - Its Use and
 * Implementation as a Programming Tool,
 * Technical Note 290, SRI International, 1983
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

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

% ops8(-Expr)
ops8(E) :-
   d((x+1)*((x^2+2)*(x^3+3)), x, E).

% divide10(-Expr)
divide10(E) :-
   d(x/x/x/x/x/x/x/x/x/x, x, E).

% log10(-Expr)
log10(E) :-
   d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).

% times10(-Expr)
times10(E) :-
   d(x*x*x*x*x*x*x*x*x*x, x, E).

% deriv
deriv :-
   ops8(_),
   divide10(_),
   log10(_),
   times10(_).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

% ops4(-Expr)
ops4(E) :-
   d((x+1)*(x^2+2), x, E).

% divide5(-Expr)
divide5(E) :-
   d(x/x/x/x/x, x, E).

% log5(-Expr)
log5(E) :-
   d(log(log(log(log(log(x))))), x, E).

% times5(-Expr)
times5(E) :-
   d(x*x*x*x*x, x, E).

% rderiv
rderiv :-
   ops4(_),
   divide5(_),
   log5(_),
   times5(_).

/*****************************************************************/
/* The Derivator                                                 */
/*****************************************************************/

% d(+Expr, +Var, -Expr)
d(U+V, X, DU+DV) :- !,
   d(U, X, DU),
   d(V, X, DV).
d(U-V, X, DU-DV) :- !,
   d(U, X, DU),
   d(V, X, DV).
d(U*V, X, DU*V+U*DV) :- !,
   d(U, X, DU),
   d(V, X, DV).
d(U/V, X, (DU*V-U*DV)/V^2) :- !,
   d(U, X, DU),
   d(V, X, DV).
d(U^N, X, DU*N*U^N1) :- !,
   N1 is N - 1,
   d(U, X, DU).
d(-U, X, -DU) :- !,
   d(U, X, DU).
d(exp(U), X, exp(U)*DU) :- !,
   d(U, X, DU).
d(log(U), X, DU/U) :- !,
   d(U, X, DU).
d(X, X, 1) :- !.
d(_, _, 0).
