/**
 * Prolog code for the arithmetic and list example.
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

/**
 * Generate a list of integers between Low and High.
 */
integers(Low, High, [Low|Rest]) :-
   Low =< High, !,
   M is Low + 1,
   integers(M, High, Rest).
integers(_, _, []).

/**
 * Remove all multiples of the given prime from a list.
 */
remove([], _, []).
remove([I|Is], P, Nis) :-
   I rem P =:= 0, !,
   remove(Is, P, Nis).
remove([I|Is], P, [I|Nis]) :-
   remove(Is, P, Nis).

/**
 * Detect primes and remove multiples.
 */
sift([I|Is], High, [I|Is]) :-
   I * I > High, !.
sift([I|Is], High, [I|Ps]) :-
   remove(Is, I, New),
   sift(New, High, Ps).

/**
 * First create the numbers and then sift.
 */
primes(High, R) :-
   integers(2, High, L),
   sift(L, High, R).
