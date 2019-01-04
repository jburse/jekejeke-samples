/**
 * The Prolog text for the testing utilities.
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

for(_).
for(N) :-
   N > 1,
   M is N-1,
   for(M).

:- meta_predicate test(?,0).
test(N, X) :-
   for(N),
   call(X), fail.
test(_, _).

show(T, G) :-
   write('\tin '),
   write(T),
   write('\t('),
   write(G),
   write(' gc) ms'), nl.

:- meta_predicate bench(?,0,?,?).
bench(M, X, T, G) :-
   uptime(T1),
   gctime(G1),
   test(M, X),
   uptime(T2),
   gctime(G2),
   T is T2-T1,
   G is G2-G1,
   write(X),
   show(T, G).

dummy.
