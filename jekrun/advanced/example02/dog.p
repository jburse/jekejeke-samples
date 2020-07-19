/**
 * Prolog code for the object oriented programming example.
 * Example 2: Prolog Slot Access
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

:- package(library(example02)).

:- module(dog, [bark/1, name/2, set_name/3]).
:- use_module(library(basic/proxy)).

% name(+Dog, -Atom)
name(Self, Name) :-
   arg(1, Self, Name).

% set_name(+Dog, +Atom, -Dog)
set_name(Self, Name, Self2) :-
   set_arg(1, Self, Name, Self2).

% bark(+Dog)
bark(Self) :-
   Self::barking(Voice),
   Self::name(Name),
   write(Name), write(' says '), write(Voice), write('.'), nl.

% ?- example02/beagle(sally)::bark.
% sally says woof.
% Yes

% ?- example02/beagle(coco)::bark.
% coco says woof.
% Yes

% ?- example02/rottweiler(zeus)::bark.
% zeus says ruff.
% Yes

% ?- example02/rottweiler(zeus)::set_name(appolo, X), X::bark.
% appolo says ruff.
% X = example02/rottweiler(coco)
