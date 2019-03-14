/**
 * Prolog code for the module mutual recursion example.
 * Modules cage in a file.
 *
 * Growth of an idealized rabbit population, according
 * to Liber Abaci from 1202 by Leonardo of Pisa, known
 * as Fibonacci.
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

:- package(library(example04)).

:- module(cage, [year/1]).
:- use_module(nest).

adults(0, 1) :- !.
adults(N, X) :-
   N > 0,
   M is N-1,
   adults(M, Y),
   babies(M, Z),
   X is Y+Z.

year(X) :-
   adults(12, X).

% ?- year(X).
% X = 233
