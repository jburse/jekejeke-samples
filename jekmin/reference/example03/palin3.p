/**
 * Prolog code for the revisited DCG with attributes example.
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

:- use_module(library(minimal/chart)).
:- use_module(library(experiment/ref)).

:- static palin/5.

palin([], [Middle]) ==>
   [Middle].
palin([Middle], []) ==>
   [Middle, Middle].
palin([Border|List], Middle) ==>
   [Border],
   palin(List, Middle),
   [Border].

% ?- use_module(library(minimal/hypo)).

% ?- <= chart("bert", _), listing('D'/2).
% :- thread_local 'D'/2.
% 'D'(116, 3).
% 'D'(114, 2).
% 'D'(101, 1).
% 'D'(98, 0).

% ?- <= chart("racecar", N), chart(palin(X1,Y1), N).
% X1 = [114,97,99],
% Y1 = [101]

% ?- <= chart("bert", N), chart(palin(X1,Y1), N).
% No