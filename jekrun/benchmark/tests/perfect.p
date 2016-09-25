/**
 * Prolog code for finding a perfect number.
 *
 * A solution that did not make use of findall/3 and rem/2
 * was posted Dec 16, 1988 on comp.lang.prolog by Thomas
 * Sj|land. It was titled Christmas pleasure and contained
 * an invitation to further dig into Euclids theorem.
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

% between1(+Integer, +Integer, -Integer)
between1(Lo, Hi, _) :-
   Lo > Hi, !, fail.
between1(Lo, _, Lo).
between1(Lo, Hi, X) :-
   Lo2 is Lo+1,
   between1(Lo2, Hi, X).

% sumlist1(+List, -Integer)
sumlist1([], 0).
sumlist1([X|Y], R) :-
   sumlist1(Y, H),
   R is X+H.

% perfect(+Integer, -Integer)
perfect(Hi, X) :-
   between1(1, Hi, X),
   Y is X//2,
   findall(Z, (  between1(1, Y, Z),
                 X rem Z =:= 0), L),
   sumlist1(L, X).

% perfect
perfect :-
   perfect(500, _).
