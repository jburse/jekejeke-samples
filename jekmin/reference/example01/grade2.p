/**
 * Bonner's second counter factual example.
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

:- op(1200, xfx, -::).
:- meta_predicate (-1-::0).

/* embedded contraction */
(- A -:: B) :- !,
   clause_ref(A, true, R),
   sys_retire_ref(R),
   call(B),
   sys_assume_ref(R).
/* embedded implication */
(A -:: B) :-
   consultable_ref(A, R),
   sys_assume_ref(R),
   call(B),
   sys_retire_ref(R).

/* student must take2 german and can choose between french and italian */
grade2(S) :-
   take2(S, german),
   take2(S, french).
grade2(S) :-
   take2(S, german),
   take2(S, italian).

/* hans has already taken german, french and italian*/
:- multifile take2/2.
take2(hans, french).

take2(anna, german).
take2(anna, french).
take2(anna, italian).

% /* hans would not grade if he takes also italian */
% ?- take2(hans, italian) -:: grade2(hans).
% No

% /* hans would grade if he takes also german */
% ?- take2(hans, german) -:: grade2(hans).
% Yes ;
% No

% /* anna would still grade if she would not have taken italian */
% ?- -take2(anna, italian) -:: grade2(anna).
% Yes ;
% No

% /* anna would not grade if she would not have taken german */
% ?- -take2(anna, german) -:: grade2(anna).
% No