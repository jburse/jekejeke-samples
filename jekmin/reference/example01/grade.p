/**
 * Bonner's first hypothetical reasoning example.
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

:- use_module(library(minimal/hypo)).

/* must take german and can choose between french and italian */
grade(S) :-
   take(S, german),
   take(S, french).
grade(S) :-
   take(S, german),
   take(S, italian).

/* hans has already taken french */
:- multifile take/2.
:- thread_local take/2.
take(hans, french).

% ?- use_module(library(minimal/hypo)).
% % 0 consults and 0 unloads in 0 ms.
% Yes

% /* hans would not grade if he takes also italian */
% ?- assumez(take(hans, italian)), grade(hans).
% No

% /* hans would grade if he takes also german */
% ?- assumez(take(hans, german)), grade(hans).
% Yes ;
% No