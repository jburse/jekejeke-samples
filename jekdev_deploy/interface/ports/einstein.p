/**
 * Prolog code for the Einstein riddle.
 *
 * First published as "Who owns the Zebra?" in the Life International
 * magazine on December 17, 1962 with solution given in the March 25,
 * 1963 issue. The riddle given here is not a verbatim copy.
 *
 * Adapted from Bill Clementson solution for Allegro Prolog:
 * https://www.artima.com/forums/flat.jsp?forum=122&thread=71585
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

% member(-Elem, +List)
member(X, [X|_]).
member(X, [_|Y]) :-
   member(X, Y).

% rightTo(-Elem, -Elem, +List)
rightTo(L, R, [L, R|_]).
rightTo(L, R, [_|Rest]) :-
   rightTo(L, R, Rest).

% nextTo(-Elem, -Elem, +List)
nextTo(X, Y, List) :-
   rightTo(X, Y, List).
nextTo(X, Y, List) :-
   rightTo(Y, X, List).

% einstein(-List)
einstein(Houses) :-
   Houses = [house(norwegian, _, _, _, _), _, house(_, _, _, milk, _), _, _],
   member(house(brit, _, _, _, red), Houses),
   member(house(swede, dog, _, _, _), Houses),
   member(house(dane, _, _, tea, _), Houses),
   rightTo(house(_, _, _, _, green), house(_, _, _, _, white), Houses),
   member(house(_, _, _, coffee, green), Houses),
   member(house(_, bird, pallmall, _, _), Houses),
   member(house(_, _, dunhill, _, yellow), Houses),
   nextTo(house(_, _, dunhill, _, _), house(_, horse, _, _, _), Houses),
   nextTo(house(_, _, marlboro, _, _), house(_, cat, _, _, _), Houses),
   nextTo(house(_, _, marlboro, _, _), house(_, _, _, water, _), Houses),
   member(house(_, _, winfield, beer, _), Houses),
   member(house(german, _, rothmans, _, _), Houses),
   nextTo(house(norwegian, _, _, _, _), house(_, _, _, _, blue), Houses),
   member(house(_, fish, _, _, _), Houses).

% test
test :- einstein(_).

% ?- einstein(X), member(Y,X), (Y=..[_|T], member(Z,T),
%    write(Z), write('\t'), fail; true), nl, fail; true.
% norwegian	cat	dunhill	water	yellow
% dane	horse	marlboro	tea	blue
% brit	bird	pallmall	milk	red
% german	fish	rothmans	coffee	green
% swede	dog	winfield	beer	white
% Yes