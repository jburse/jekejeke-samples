/**
 * Simulation of some Jekejeke-Prolog number conversion.
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

:- module(conversion, [hex_codes/2]).

/**
 * hex_codes(I, L):
 * If L is a list of codes, the predicates succeeds in I
 * with the hex integer represented by the codes L. Otherwise
 * the predicate succeeds in L with the codes of the
 * hex integer I.
 */
% hex_codes(-+Integer, +-List)
hex_codes(I, L) :- ground(L), !,
   codes_to_hex(L, I).
hex_codes(I, L) :-
   hex_to_codes(I, L).

codes_to_hex([0'-|L], I) :- !,
   codes_to_hex(L, 0, J),
   I is -J.
codes_to_hex(L, I) :-
   codes_to_hex(L, 0, I).

codes_to_hex([C|L], I, J) :-
   code_type(C, xdigit(D)),
   H is I*16+D,
   codes_to_hex(L, H, J).
codes_to_hex([], I, I).

hex_to_codes(I, L) :- I < 0, !,
   J is -I,
   hex_to_codes(J, [], R),
   L = [0'-|R].
hex_to_codes(0, L) :- !,
   L = [0'0].
hex_to_codes(I, L) :-
   hex_to_codes(I, [], L).

hex_to_codes(0, L, R) :- !,
   R = L.
hex_to_codes(I, L, R) :-
   D is I mod 16,
   code_type(C, xdigit(D)),
   J is I//16,
   hex_to_codes(J, [C|L], R).
