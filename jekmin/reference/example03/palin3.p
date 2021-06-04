/**
 * Palindromes in via chart DCG.
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

:- use_module(library(minimal/chart)).

:- multifile 'D'/3.
:- thread_local 'D'/3, palin/4.
:- forward palin/5.

palin([], [Middle]) ==:
   [Middle].
palin([Middle], []) ==:
   [Middle, Middle].
palin([Border|List], Middle) ==:
   [Border], palin(List, Middle), [Border].

% ?- use_module(library(minimal/chart)).
% % 0 consults and 0 unloads in 0 ms.
% Yes

% ?- words("bert", 0, _), listing('D'/3).
% :- thread_local 'D'/3.
% 'D'(116, 3, 4).
% 'D'(114, 2, 3).
% 'D'(101, 1, 2).
% 'D'(98, 0, 1).

% ?- words("racecar", 0, N), chart(palin(X,Y), 0, N).
% X = [114,97,99],
% Y = [101]

% ?- words("bert", 0, N), chart(palin(X,Y), 0, N).
% No