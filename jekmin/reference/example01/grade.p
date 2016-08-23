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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).

:- op(1200, xfx, -:).
:- meta_predicate (-1-:0).

/* embedded implication */
(A -: B) :-
   consultable_ref(A, R),
   sys_assume_ref(R),
   call(B),
   sys_retire_ref(R).

/* student must take german and can choose between french and italian */
grade(S) :-
   take(S, german),
   take(S, french).
grade(S) :-
   take(S, german),
   take(S, italian).

/* hans has already taken french */
:- multifile take/2.
take(hans, french).

% /* hans would not grade if he takes also italian */
% ?- take(hans, italian) -: grade(hans).
% No

% /* hans would grade if he takes also german */
% ?- take(hans, german) -: grade(hans).
% Yes ;
% No