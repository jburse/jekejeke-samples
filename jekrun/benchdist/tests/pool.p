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
:- use_module(library(experiment/ref)).

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

pool :- create,
   remove(X),
   collatz(X, _).

pool2 :- create,
   horde(X^(  remove(X),
              collatz(X, _)), 2).

pool4 :- create,
   horde(X^(  remove(X),
              collatz(X, _)), 4).

pool8 :- create,
   horde(X^(  remove(X),
              collatz(X, _)), 8).

gotcha :-
   once((  create,
           remove(X),
           collatz(X, _),
           X = 16666)).

gotcha2 :-
   once((  create,
           horde(X^(  remove(X),
                      collatz(X, _),
                      X = 16666), 2))).

gotcha4 :-
   once((  create,
           horde(X^(  remove(X),
                      collatz(X, _),
                      X = 16666), 4))).

gotcha8 :-
   once((  create,
           horde(X^(  remove(X),
                      collatz(X, _),
                      X = 16666), 8))).

/*****************************************************************/
/* Pool Creation                                                 */
/*****************************************************************/

:- dynamic pool/2.
:- dynamic subpool/3.

% create
create :-
   call_cleanup(
      sys_setup_create,
      sys_fini_create).

% sys_setup_create
% Leave a choice point!
sys_setup_create :-
   between(454, 909, Y),
   A is Y//22,
   B is Y rem 22,
   assertz(pool(A, B)), fail.
sys_setup_create :-
   between(10000, 20000, X),
   Y is X//22,
   C is X rem 22,
   A is Y//22,
   B is Y rem 22,
   assertz(subpool(A, B, C)), fail.
sys_setup_create.
sys_setup_create :- fail.

% sys_fini_create
sys_fini_create :-
   retractall(pool(_, _)),
   retractall(subpool(_, _, _)).

% remove(-Integer)
remove(X) :-
   retract_alt(pool(A,B)),
   Y is A*22+B,
   retract_alt(subpool(A,B,C)),
   X is Y*22+C.

% retract_alt(+Head)
retract_alt(H) :-
   clause_ref(H, true, R),
   erase_ref(R).
