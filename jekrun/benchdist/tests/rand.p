/**
 * Reproducable and distributable random numbers.
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

:- use_module(library(basic/random)).
:- use_module(library(advanced/arith)).
:- use_module(library(runtime/distributed)).
:- use_module(library(misc/aggregate)).

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

rand :-
   seeds(120, X),
   aggregate_all(sum(Y), sequence(120, X, Y), _).

rand2 :-
   balance((  seeds(120, X),
              aggregate_all(sum(Y), sequence(120, X, Y), _)), 2).

rand4 :-
   balance((  seeds(120, X),
              aggregate_all(sum(Y), sequence(120, X, Y), _)), 4).

rand8 :-
   balance((  seeds(120, X),
              aggregate_all(sum(Y), sequence(120, X, Y), _)), 8).

/*****************************************************************/
/* Some Helpers                                                  */
/*****************************************************************/

% seeds(+Integer, -Integer)
seeds(N, X) :-
   random_new(666, R),
   between(1, N, _),
   random_next(R, 1000, X).

% seeds(+Integer, +Integer, -Integer)
sequence(N, S, X) :-
   T is S*1000,
   random_new(T, R),
   between(1, N, _),
   random_next(R, 1000, X).
