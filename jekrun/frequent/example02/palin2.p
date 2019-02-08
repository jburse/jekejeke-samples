/**
 * Prolog code for the DCG with attributes example.
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

:- use_module(library(standard/dcg)).

palin([], [Middle]) -->
   [Middle].
palin([Middle], []) -->
   [Middle,
    Middle].
palin([Border|List], Middle) -->
   [Border],
   palin(List, Middle),
   [Border].

% ?- phrase(palin(X1, Y1), "racecar").
% X1 = [114, 97, 99],
% Y1 = [101]

% ?- phrase(palin(X1, Y1), "anna").
% X1 = [97, 110],
% Y1 = []

% ?- phrase(palin(X1, Y1), "bert").
% No

% ?- phrase(palin("ra", "d"), X), atom_codes(A,X).
% X = [114,97,100,97,114],
% A = radar