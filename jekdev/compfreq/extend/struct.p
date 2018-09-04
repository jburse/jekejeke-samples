/**
 * Prolog code for the Prolog dict test cases.
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

:- use_module(library(advanced/arith)).
:- use_module(library(basic/lists)).
:- use_module(library(advanced/dict)).

runner:ref(sys_struct, 2, extend_struct, 'SWI7 2.1').
runner:case(sys_struct, 2, extend_struct, 'SWI7 2.1, XLOG 1') :-
   X = point{y:2,x:1},
   X == point{x:1,y:2}.
runner:case(sys_struct, 2, extend_struct, 'SWI7 2.1, XLOG 2') :-
   point{x:1,y:2} = point{y:2,x:1}.
runner:case(sys_struct, 2, extend_struct, 'SWI7 2.1, XLOG 3') :-
   point{y:2,x:1} = _{M},
   M == (x:1,y:2).

runner:ref(is_dict, 1, extend_struct, 'SWI7 2.2').
runner:case(is_dict, 1, extend_struct, 'SWI7 2.2, XLOG 1') :-
   \+ is_dict(_).
runner:case(is_dict, 1, extend_struct, 'SWI7 2.2, XLOG 2') :-
   is_dict(_{x:1,y:2}).
runner:case(is_dict, 1, extend_struct, 'SWI7 2.2, XLOG 3') :-
   is_dict(point{}).
runner:case(is_dict, 1, extend_struct, 'SWI7 2.2, XLOG 4') :-
   \+ is_dict(123).

runner:ref(is_dict, 2, extend_struct, 'SWI7 2.3').
runner:case(is_dict, 2, extend_struct, 'SWI7 2.3, XLOG 1') :-
   \+ is_dict(_, _).
runner:case(is_dict, 2, extend_struct, 'SWI7 2.3, XLOG 2') :-
   is_dict(S{x:1,y:2}, T),
   T == S.
runner:case(is_dict, 2, extend_struct, 'SWI7 2.3, XLOG 3') :-
   is_dict(point{}, T),
   T == point.
runner:case(is_dict, 2, extend_struct, 'SWI7 2.3, XLOG 4') :-
   \+ is_dict(foo, _).

runner:ref(dict_pairs, 3, extend_struct, 'SWI7 2.4').
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 1') :-
   catch(dict_pairs(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 2') :-
   dict_pairs(S{x:1,y:2}, T, L),
   T == S,
   L == [x-1,y-2].
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 3') :-
   dict_pairs(point{}, T, L),
   T == point,
   L == [].
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 4') :-
   dict_pairs(S, T, [x-1,y-2]),
   S == T{x:1,y:2}.
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 5') :-
   dict_pairs(S, point, []),
   S == point{}.
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 6') :-
   catch(dict_pairs(_, point, [x-1,y-2,x-3]), error(E,_), true),
   E == domain_eror(duplicate_key,x).
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 7') :-
   catch(dict_pairs(_, point, [x-1,_-2]), error(E,_), true),
   E == instantiation_error.
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 8') :-
   \+ dict_pairs(_{x:1,y:2}, _, [x-1,z-3]).
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 9') :-
   \+ dict_pairs(point{}, color, _).
runner:case(dict_pairs, 3, extend_struct, 'SWI7 2.4, XLOG 10') :-
   catch(dict_pairs(123, _, _), error(E,_), true),
   E == type_error(dict,123).

runner:ref(get_dict, 3, extend_struct, 'SWI7 2.5').
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 1') :-
   catch(get_dict(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 2') :-
   get_dict(y, _{x:1,y:X}, 2),
   X == 2.
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 3') :-
   \+ get_dict(y, _{x:1,y:2}, 3).
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 4') :-
   \+ get_dict(z, _{x:1,y:2}, _).
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 5') :-
   \+ get_dict(z, point{}, _).
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 6') :-
   catch(get_dict(_, 123, _), error(E,_), true),
   E == type_error(dict,123).
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 7') :-
   findall(V, get_dict(_, _{x:1,y:2}, V), [_,X|_]),
   X == 2.
runner:case(get_dict, 3, extend_struct, 'SWI7 2.5, XLOG 8') :-
   findall(K, get_dict(K, _{x:1,y:2}, _), [X|_]),
   X == x.

runner:ref(:<, 2, extend_struct, 'SWI7 2.6').
runner:case(:<, 2, extend_struct, 'SWI7 2.6, XLOG 1') :-
   T{x:V} :< point{x:1,y:2},
   T == point,
   V == 1.
runner:case(:<, 2, extend_struct, 'SWI7 2.6, XLOG 2') :-
   point{x:V,y:2} :< T{x:1,y:W},
   V == 1,
   T == point,
   W == 2.
runner:case(:<, 2, extend_struct, 'SWI7 2.6, XLOG 3') :-
   point{} :< T{x:1,y:2},
   T == point.
runner:case(:<, 2, extend_struct, 'SWI7 2.6, XLOG 4') :-
   \+ point{x:_,y:_} :< _{}.
runner:case(:<, 2, extend_struct, 'SWI7 2.6, XLOG 5') :-
   \+ _{x:3} :< point{x:1,y:2}.
runner:case(:<, 2, extend_struct, 'SWI7 2.6, XLOG 6') :-
   \+ point{} :< colorpoint{}.

runner:ref(del_dict, 4, extend_struct, 'SWI7 2.7').
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 1') :-
   catch(del_dict(_, _, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 2') :-
   del_dict(y, S{x:1,y:X}, 2, T),
   X == 2,
   T == S{x:1}.
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 3') :-
   \+ del_dict(y, _{x:1,y:2}, 3, _).
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 4') :-
   \+ del_dict(z, _{x:1,y:2}, _, _).
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 5') :-
   \+ del_dict(z, point{}, _, _).
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 6') :-
   \+ del_dict(y, S{x:1,y:2}, _, S{x:3}).
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 7') :-
   catch(del_dict(_, 123, _, _), error(E,_), true),
   E == type_error(dict,123).
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 8') :-
   findall(V, del_dict(_, _{x:1,y:2}, V, _), [_,X|_]),
   X == 2.
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 9') :-
   findall(K, del_dict(K, _{x:1,y:2}, _, _), [X|_]),
   X == x.
runner:case(del_dict, 4, extend_struct, 'SWI7 2.7, XLOG 10') :-
   findall(T, del_dict(_, point{x:1,y:2}, _, T), [_,X|_]),
   X == point{x:1}.

runner:ref(put_dict, 4, extend_struct, 'SWI7 2.8').
runner:case(put_dict, 2, extend_struct, 'SWI7 2.8, XLOG 1') :-
   catch(put_dict(_, _, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(put_dict, 2, extend_struct, 'SWI7 2.8, XLOG 2') :-
   put_dict(y, S{x:1,y:2}, X, T),
   T == S{y:X,x:1}.
runner:case(put_dict, 2, extend_struct, 'SWI7 2.8, XLOG 3') :-
   put_dict(z, S{x:1,y:2}, X, T),
   T == S{z:X,x:1,y:2}.
runner:case(put_dict, 2, extend_struct, 'SWI7 2.8, XLOG 4') :-
   put_dict(z, point{}, X, T),
   T == point{z:X}.
runner:case(put_dict, 2, extend_struct, 'SWI7 2.8, XLOG 5') :-
   \+ put_dict(y, S{x:1,y:2}, X, S{z:X,x:1}).
runner:case(put_dict, 2, extend_struct, 'SWI7 2.8, XLOG 6') :-
   catch(put_dict(x, foo, _, _), error(E,_), true),
   E == type_error(dict,foo).
