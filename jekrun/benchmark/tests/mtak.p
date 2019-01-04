/**
 * Prolog code for the tak function benchmark.
 *
 * McCarthys tak function.
 * Modification of the tarai function from Takeuchi.
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

% fun(+Integer, +Integer, +Integer, -Integer)
fun(X, Y, Z, A) :-
   X =< Y, !,
   Z = A.
fun(X, Y, Z, A) :-
   X1 is X-1,
   fun(X1, Y, Z, A1),
   Y1 is Y-1,
   fun(Y1, Z, X, A2),
   Z1 is Z-1,
   fun(Z1, X, Y, A3),
   fun(A1, A2, A3, A).

% mtak
mtak :-
   fun(18, 12, 6, _).
