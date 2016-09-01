/**
 * Prolog code for the consult apply theory test cases.
 *
 * Source of test cases is the following standard:
 *   - Draft Technical Corrigendum 2, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2</a>
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

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

/****************************************************************/
/* Higher Order                                                 */
/****************************************************************/

runner:ref(call, 0, consult_apply, 'Corr.2 8.15.4.4').

:- meta_predicate maplist(1,?).
maplist(_, []).
maplist(F, [X|Y]) :-
   call(F, X),
   maplist(F, Y).

makerec(rec(X,Y,Z,U,V,W), X, Y, Z, U, V, W).

runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 1') :-
   call(integer, 3).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 2') :-
   call(functor(F, c), 0),
   F == c.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 3') :-
   call(call(call(atom_concat, pro), log), Atom),
   Atom == prolog.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 4a') :-
   call(;, X=1, _=2),
   X == 1.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 4b') :-
   findall(Y, call(;, _=1, Y=2), [_,Y|_]),
   Y == 2.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 1') :-
   \+ findall(Y, call(;, (_=1,!), Y=2), [_,Y|_]).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 2') :-
   findall(Y, (  call(',', _=1, !)
              ;  Y = 2), [_,Y|_]),
   Y == 2.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 5') :-
   \+ call(;, (true->fail), _=1).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 6') :-
   maplist(>(3), [1,2]).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 7') :-
   \+ maplist(>(3), [1,23]).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 8a') :-
   maplist(=(X), [1,1]),
   X == 1.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 8b') :-
   \+ maplist(=(_), [1,2]).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, ISO 8c') :-
   maplist(=(_), [Y,Z]),
   Y == Z.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 3') :-
   catch(call(',', atom(1), 3), error(E,_), true),
   E == type_error(callable,(atom(1),3)).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 4') :-
   catch(call(',', integer(1), 3), error(E,_), true),
   E == type_error(callable,(integer(1),3)).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 5') :-
   catch(call(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 6') :-
   catch(call(3, _), error(E,_), true),
   E == type_error(callable,3).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 7') :-
   call(call(call(call(call(call(call(makerec, S), X), Y), Z), U), V), W),
   S == rec(X,Y,Z,U,V,W).
