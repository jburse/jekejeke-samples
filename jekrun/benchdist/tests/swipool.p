/**
 * Balanced execution of pool extraction.
 * SWI-Prolog version.
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

:- ensure_loaded('../compat/swidistributed').

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

pool :-
   makepool(10000, 20000), retract(pool(X)), collatz(X, _).

pool2 :-
   makepool(10000, 20000), balance(retract(pool(X)), collatz(X, _), 2).

pool4 :-
   makepool(10000, 20000), balance(retract(pool(X)), collatz(X, _), 4).

pool8 :-
   makepool(10000, 20000), balance(retract(pool(X)), collatz(X, _), 8).

/*****************************************************************/
/* Pool Creation                                                 */
/*****************************************************************/

% makepool(+Integer, +Integer)
makepool(F, T) :-
   between(F, T, X),
   assertz(pool(X)),
   fail.
makepool(_, _).
