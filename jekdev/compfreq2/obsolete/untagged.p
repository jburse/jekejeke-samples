/**
 * Prolog code for the untagged structure test cases.
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

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(advanced/json)).
:- set_prolog_flag(double_quotes, string).

/**********************************************************/
/* JSON Objects                                           */
/**********************************************************/

/* {} and {M} */

runner:ref(sys_json, 2, obsolete_untagged, 'JSON 2.1').
runner:case(sys_json, 2, obsolete_untagged, 'JSON 2.1, XLOG 1') :-
   X = {"y":2,"x":1},
   \+ X == {"x":1,"y":2}.
runner:case(sys_json, 2, obsolete_untagged, 'JSON 2.1, XLOG 2') :-
   \+ {"y":2,"x":1} == {"x":1,"y":2}.
runner:case(sys_json, 2, obsolete_untagged, 'JSON 2.1, XLOG 3') :-
   {"y":2,"x":1} = {M},
   M = ("y":2,"x":1).
runner:case(sys_json, 2, obsolete_untagged, 'JSON 2.1, XLOG 4') :-
   \+ {} = {"x":1}.

/* is_json(D) */

runner:ref(is_json, 1, obsolete_untagged, 'JSON 2.2').
runner:case(is_json, 1, obsolete_untagged, 'JSON 2.2, XLOG 1') :-
   \+ is_json(_).
runner:case(is_json, 1, obsolete_untagged, 'JSON 2.2, XLOG 2') :-
   is_json({"x":1,"y":2}).
runner:case(is_json, 1, obsolete_untagged, 'JSON 2.2, XLOG 3') :-
   is_json({}).
runner:case(is_json, 1, obsolete_untagged, 'JSON 2.2, XLOG 4') :-
   \+ is_json(123).

/* get_json(K,D,V) */

runner:ref(get_json, 3, obsolete_untagged, 'JSON 2.3').
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 1') :-
   catch(get_json(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 2') :-
   get_json("y", {"x":1,"y":X}, 2),
   X == 2.
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 3') :-
   \+ get_json("y", {"x":1,"y":2}, 3).
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 4') :-
   \+ get_json("z", {"x":1,"y":2}, _).
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 5') :-
   \+ get_json("z", {}, _).
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 6') :-
   catch(get_json(_, 123, _), error(E,_), true),
   E = type_error(json,123).
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 7') :-
   findall(V, get_json(_, {"x":1,"y":2}, V), [_,X|_]),
   X == 2.
runner:case(get_json, 3, obsolete_untagged, 'JSON 2.3, XLOG 8') :-
   findall(K, get_json(K, {"x":1,"y":2}, _), [X,_|_]),
   X == "x".

/* select_json(E,D,R) */

runner:ref(select_json, 3, obsolete_untagged, 'JSON 2.4').
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 1') :-
   catch(select_json(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 2') :-
   select_json({"y":2}, {"x":1,"y":X}, T),
   X == 2,
   T == {"x":1}.
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 3') :-
   \+ select_json({"y":3}, {"x":1,"y":2}, _).
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 4') :-
   \+ select_json({"z":_}, {"x":1,"y":2}, _).
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 5') :-
   \+ select_json({"z":_}, {}, _).
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 6') :-
   \+ select_json({"y":_}, {"x":1,"y":2}, {"x":3}).
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 7') :-
   catch(select_json(_, 123, _), error(E,_), true),
   E == type_error(json,123).
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 8') :-
   select_json({"x":1,"y":2}, {"u":0,"x":1,"y":2,"z":3}, T),
   T = {"u":0,"z":3}.
runner:case(select_json, 3, obsolete_untagged, 'JSON 2.4, XLOG 9') :-
   catch(select_json(foo, {}, _), error(E,_), true),
   E == type_error(json,foo).

/* del_json(K,D,V,R) */

runner:ref(del_json, 4, obsolete_untagged, 'JSON 2.5').
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 1') :-
   catch(del_json(_, _, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 2') :-
   del_json("y", {"x":1,"y":X}, 2, T),
   X == 2,
   T == {"x":1}.
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 3') :-
   \+ del_json("y", {"x":1,"y":2}, 3, _).
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 4') :-
   \+ del_json("z", {"x":1,"y":2}, _, _).
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 5') :-
   \+ del_json("z", {}, _, _).
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 6') :-
   \+ del_json("y", {"x":1,"y":2}, _, {"x":3}).
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 7') :-
   catch(del_json(_, 123, _, _), error(E,_), true),
   E == type_error(json,123).
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 8') :-
   findall(V, del_json(_, {"x":1,"y":2}, V, _), [_,X|_]),
   X == 2.
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 9') :-
   findall(K, del_json(K, {"x":1,"y":2}, _, _), [X|_]),
   X == "x".
runner:case(del_json, 4, obsolete_untagged, 'JSON 2.5, XLOG 10') :-
   findall(T, del_json(_, {"x":1,"y":2}, _, T), [_,X|_]),
   X == {"x":1}.

/* put_json(E,D,R) */

runner:ref(put_json, 3, obsolete_untagged, 'JSON 2.6').
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 1') :-
   catch(put_json(_, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 2') :-
   put_json({"y":X}, {"x":1,"y":2}, T),
   T == {"x":1,"y":X}.
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 3') :-
   put_json({"z":X}, {"x":1,"y":2}, T),
   T == {"x":1,"y":2,"z":X}.
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 4') :-
   put_json({"z":X}, {}, T),
   T == {"z":X}.
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 5') :-
   \+ put_json({"y":X}, {"x":1,"y":2}, {"z":X,"x":1}).
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 6') :-
   catch(put_json(_, foo, _), error(E,_), true),
   E == type_error(json,foo).
runner:case(put_json, 3, obsolete_untagged, 'JSON 2.6, XLOG 7') :-
   put_json({"u":0,"x":4,"z":5}, {"x":1,"y":2,"z":3}, T),
   T == {"x":4,"y":2,"z":5,"u":0}.

/* put_json(K,D,V,R) */

runner:ref(put_json, 4, obsolete_untagged, 'JSON 2.7').
runner:case(put_json, 4, obsolete_untagged, 'JSON 2.7, XLOG 1') :-
   catch(put_json(_, _, _, _), error(E,_), true),
   E == instantiation_error.
runner:case(put_json, 4, obsolete_untagged, 'JSON 2.7, XLOG 2') :-
   put_json("y", {"x":1,"y":2}, X, T),
   T == {"x":1,"y":X}.
runner:case(put_json, 4, obsolete_untagged, 'JSON 2.7, XLOG 3') :-
   put_json("z", {"x":1,"y":2}, X, T),
   T == {"x":1,"y":2,"z":X}.
runner:case(put_json, 4, obsolete_untagged, 'JSON 2.7, XLOG 4') :-
   put_json("z", {}, X, T),
   T == {"z":X}.
runner:case(put_json, 4, obsolete_untagged, 'JSON 2.7, XLOG 5') :-
   \+ put_json("y", {"x":1,"y":2}, X, {"z":X,"x":1}).
runner:case(put_json, 4, obsolete_untagged, 'JSON 2.7, XLOG 6') :-
   catch(put_json("x", foo, _, _), error(E,_), true),
   E = type_error(json,foo).
