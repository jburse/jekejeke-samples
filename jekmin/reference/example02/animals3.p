/**
 * Animals expert system via forward chaining.
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

:- use_module(library(minimal/delta)).

:- multifile motion/1, skin/1, diet/1.
:- thread_local motion/1, skin/1, class/1, diet/1, animal/1.

post(class(mamal)) <= posted(motion(walk)), posted(skin(fur)).
post(class(fish)) <= posted(motion(swim)), posted(skin(scale)).
post(class(bird)) <= posted(motion(fly)), posted(skin(feather)).

post(animal(rodent)) <= posted(class(mamal)), posted(diet(plant)).
post(animal(cat)) <= posted(class(mamal)), posted(diet(meat)).
post(animal(salmon)) <= posted(class(fish)), posted(diet(meat)).
post(animal(eagle)) <= posted(class(bird)), posted(diet(meat)).

write('The animal is '), write(X), nl <= posted(animal(X)).

% ?- use_module(library(minimal/delta)).
% % 0 consults and 0 unloads in 0 ms.
% Yes

% ?- post(motion(walk)), post(skin(fur)).
% Yes

% ?- post(motion(walk)), post(skin(fur)), post(diet(meat)).
% The animal is cat
% Yes

% ?- post(motion(walk)), post(skin(fur)), post(diet(plant)).
% The animal is rodent
% Yes