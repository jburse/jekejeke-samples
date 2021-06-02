/**
 * Prolog code for the object oriented programming example.
 * Example 4: Call-Out Java Object
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

:- package(library(example04)).

:- module(main, [dog_bark/1, beagle_new/1, rottweiler_new/1]).

:- virtual dog_bark/1.
:- foreign(dog_bark/1, example04/'Dog', bark).

:- foreign_constructor(beagle_new/1, example04/'Beagle', new).

:- foreign_constructor(rottweiler_new/1, example04/'Rottweiler', new).

% ?- beagle_new(X), dog_bark(X).
% example04.Beagle@37b37c04 says woof.
% X = 0r37b37c04

% ?- rottweiler_new(X), dog_bark(X).
% example04.Rottweiler@129aa3b9 says ruff.
% X = 0r129aa3b9