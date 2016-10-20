/**
 * Prolog code for the logic example.
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

motion(walk).
skin(fur).
diet(meat).

class(mamal) :-
   motion(walk),
   skin(fur).
class(fish) :-
   motion(swim),
   skin(scale).
class(bird) :-
   motion(fly),
   skin(feather).

animal(rodent) :-
   class(mamal),
   diet(plant).
animal(cat) :-
   class(mamal),
   diet(meat).
animal(salmon) :-
   class(fish),
   diet(meat).
animal(eagle) :-
   class(bird),
   diet(meat).

% ?- animal(X)
% X = cat ;
% no
