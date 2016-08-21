/**
 * Balanced execution of the collatz function.
 * Jekejeke Prolog version.
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

:- use_module(library(runtime/distributed)).
:- use_module(library(advanced/arith)).

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

many :-
   between(10000, 20000, X), collatz(X, _).

many2 :-
   balance(between(10000, 20000, X), collatz(X, _), 2).

many4 :-
   balance(between(10000, 20000, X), collatz(X, _), 4).

many8 :-
   balance(between(10000, 20000, X), collatz(X, _), 8).

first :-
   once((between(10000, 20000, X), collatz(X, _), X = 16666)).

first2 :-
   once(balance(between(10000, 20000, X), (collatz(X, _), X = 16666), 2)).

first4 :-
   once(balance(between(10000, 20000, X), (collatz(X, _), X = 16666), 4)).

first8 :-
   once(balance(between(10000, 20000, X), (collatz(X, _), X = 16666), 8)).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

rmany :-
   between(1000, 2000, X), collatz(X, _).

rmany2 :-
   balance(between(1000, 2000, X), collatz(X, _), 2).

rmany4 :-
   balance(between(1000, 2000, X), collatz(X, _), 4).

rmany8 :-
   balance(between(1000, 2000, X), collatz(X, _), 8).

rfirst :-
   once((between(1000, 2000, X), collatz(X, _), X = 1666)).

rfirst2 :-
   once(balance(between(1000, 2000, X), (collatz(X, _), X = 1666), 2)).

rfirst4 :-
   once(balance(between(1000, 2000, X), (collatz(X, _), X = 1666), 4)).

rfirst8 :-
   once(balance(between(1000, 2000, X), (collatz(X, _), X = 1666), 8)).

/*****************************************************************/
/* The Collatz Function                                          */
/*****************************************************************/

collatz(1, 0) :- !.
collatz(I, N) :- 1 =:= I/\1, !,
   I0 is I*3+1, collatz(I0, N0), N is N0+1.
collatz(I, N) :-
   I0 is I>>1,  collatz(I0, N0), N is N0+1.
