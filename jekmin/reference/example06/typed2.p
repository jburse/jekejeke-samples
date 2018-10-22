/**
 * Prolog code for the type inference
 * with subject to occurs check.
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

:- use_module(library(experiment/maps)).
:- use_module(library(term/herbrand)).

/*******************************************************/
/* Type Inference for Simple Types                     */
/*******************************************************/

% typed2(+Expression, +Context, -Type)
typed2(X, C, T) :-
   var(X), !,
   get(C, X, T).
typed2(lam(X,E), C, (S->T)) :-
   typed2(E, [X-S|C], T).
typed2(app(E,F), C, T) :-
   sto(S),
   typed2(E, C, (S->T)),
   typed2(F, C, S).

% ?- sto((A,B,C)), typed2(app(E,F), [E-A,F-B], C).
% A = (B->C),
% sto(C),
% sto(B)

% ?- sto((A,B)), typed2(app(F,F), [F-A], B).
% No

% ?- sto(A), typed2(lam(X,lam(Y,app(Y,X))), [], A).
% A = (_A->(_A->_B)->_B),
% sto(_B),
% sto(_A)