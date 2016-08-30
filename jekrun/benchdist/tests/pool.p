/**
 * Balanced execution of pool extraction.
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

pool :-
   make(10000, 20000),
   remove(X),
   collatz(X, _).

pool2 :-
   make(10000, 20000),
   horde((  remove(X),
            collatz(X, _)), 2).

pool4 :-
   make(10000, 20000),
   horde((  remove(X),
            collatz(X, _)), 4).

pool8 :-
   make(10000, 20000),
   horde((  remove(X),
            collatz(X, _)), 8).

gotcha :-
   once((  make(10000, 20000),
           remove(X),
           collatz(X, _),
           X = 16666)).

gotcha2 :-
   once((  make(10000, 20000),
           horde((  remove(X),
                    collatz(X, _),
                    X = 16666), 2))).

gotcha4 :-
   once((  make(10000, 20000),
           horde((  remove(X),
                    collatz(X, _),
                    X = 16666), 4))).

gotcha8 :-
   once((  make(10000, 20000),
           horde((  remove(X),
                    collatz(X, _),
                    X = 16666), 8))).

/*****************************************************************/
/* Pool Creation                                                 */
/*****************************************************************/

% make(+Integer, +Integer)
make(F, T) :-
   call_cleanup(
      sys_setup_make(F, T),
      sys_fini_make).

% sys_setup_make(+Integer, +Integer)
% Leave a choice point!
sys_setup_make(F, T) :-
   between(F, T, X),
   assertz(pool(X)), fail.
sys_setup_make(_, _).
sys_setup_make(_, _) :- fail.

% sys_fini_make
sys_fini_make :-
   retractall(pool(_)).

% remove(-Integer)
remove(X) :-
   retract(pool(X)).
