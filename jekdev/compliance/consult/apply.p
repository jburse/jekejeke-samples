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

:- use_module(library(advanced/arith)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

/****************************************************************/
/* Higher Order                                                 */
/****************************************************************/

/* call/n */

makerec(rec(X,Y,Z,U,V,W), X, Y, Z, U, V, W).

runner:ref(call, 0, consult_apply, 'Corr.2 8.15.4.4').
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
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 3') :-
   catch(call(',', atom(1), 3), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 4') :-
   catch(call(',', integer(1), 3), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 5') :-
   catch(call(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 6') :-
   catch(call(3, _), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 0, consult_apply, 'Corr.2 8.15.4.4, XLOG 7') :-
   call(call(call(call(call(call(call(makerec, S), X), Y), Z), U), V), W),
   S == rec(X,Y,Z,U,V,W).

/* maplist/n */

runner:ref(maplist, 0, consult_apply, 'N235 7.4').
runner:case(maplist, 0, consult_apply, 'N235 7.4, ISO 6') :-
   maplist(>(3), [1,2]).
runner:case(maplist, 0, consult_apply, 'N235 7.4, ISO 7') :-
   \+ maplist(>(3), [1,2,3]).
runner:case(maplist, 0, consult_apply, 'N235 7.4, ISO 8a') :-
   maplist(=(X), [1,1]),
   X == 1.
runner:case(maplist, 0, consult_apply, 'N235 7.4, ISO 8b') :-
   \+ maplist(=(_), [1,2]).
runner:case(maplist, 0, consult_apply, 'N235 7.4, ISO 8c') :-
   maplist(=(_), [Y,Z]),
   Y == Z.
runner:case(maplist, 0, consult_apply, 'N235 7.4, XLOG 1') :-
   maplist(succ, [1,2,3], X),
   X == [2,3,4].
runner:case(maplist, 0, consult_apply, 'N235 7.4, XLOG 2') :-
   maplist(succ, Y, [2,3,4]),
   Y == [1,2,3].
runner:case(maplist, 0, consult_apply, 'N235 7.4, XLOG 3') :-
   findall(X, maplist(between, [1,2], [2,3], X), [_,Y|_]),
   Y == [1,3].
runner:case(maplist, 0, consult_apply, 'N235 7.4, XLOG 4') :-
   \+ maplist(between, [1,2], [2,3], [2,1]).

/* foldl/n */

edge(1, a, b).
edge(2, a, c).
edge(3, c, d).
edge(4, b, e).
edge(5, d, e).

runner:ref(foldl, 0, consult_apply, 'PLIB HO Preds').
runner:case(foldl, 0, consult_apply, 'PLIB HO Preds, XLOG 1') :-
   foldl(+, [1,2,3], 0, S),
   S == 6.
runner:case(foldl, 0, consult_apply, 'PLIB HO Preds, XLOG 2') :-
   foldl(foldl(+), [[1,2,3],[4,5,6],[7,8,9]], 0, S),
   S == 45.
runner:case(foldl, 0, consult_apply, 'PLIB HO Preds, XLOG 3') :-
   \+ foldl(edge, _, d, a).
runner:case(foldl, 0, consult_apply, 'PLIB HO Preds, XLOG 4') :-
   foldl(edge, L, a, d),
   L == [2,3].
runner:case(foldl, 0, consult_apply, 'PLIB HO Preds, XLOG 5') :-
   findall(X, foldl(edge, X, a, e), [_,Y|_]),
   Y == [2,3,5].
