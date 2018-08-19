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

runner:ref(is_dict, 1, extend_struct, 'SWI7 1.1').
runner:case(is_dict, 1, extend_struct, 'SWI7 1.1, XLOG 1') :-
   \+ is_dict(_).
runner:case(is_dict, 1, extend_struct, 'SWI7 1.1, XLOG 1') :-
   is_dict(_{x:1,y:2}).
runner:case(is_dict, 1, extend_struct, 'SWI7 1.1, XLOG 1') :-
   is_dict(point{}).
runner:case(is_dict, 1, extend_struct, 'SWI7 1.1, XLOG 1') :-
   \+ is_dict(123).

runner:ref(is_dict, 2, extend_struct, 'SWI7 1.2').
runner:case(is_dict, 2, extend_struct, 'SWI7 1.2, XLOG 1') :-
   \+ is_dict(_, _).
runner:case(is_dict, 2, extend_struct, 'SWI7 1.2, XLOG 1') :-
   is_dict(S{x:1,y:2}, T),
   T == S.
runner:case(is_dict, 2, extend_struct, 'SWI7 1.2, XLOG 1') :-
   is_dict(point{}, T),
   T == point.
runner:case(is_dict, 2, extend_struct, 'SWI7 1.2, XLOG 1') :-
   \+ is_dict(foo, _).

runner:ref(dict_pairs, 3, extend_struct, 'SWI7 1.3').
runner:case(dict_pairs, 3, extend_struct, 'SWI7 1.3, XLOG 1') :- true.

runner:ref(get_dict, 3, extend_struct, 'SWI7 1.4').
runner:case(get_dict, 3, extend_struct, 'SWI7 1.4, XLOG 1') :- true.

runner:ref(:<, 2, extend_struct, 'SWI7 1.5').
runner:case(:<, 2, extend_struct, 'SWI7 1.5, XLOG 1') :- true.

runner:ref(del_dict, 4, extend_struct, 'SWI7 1.6').
runner:case(del_dict, 2, extend_struct, 'SWI7 1.6, XLOG 1') :- true.

runner:ref(put_dict, 4, extend_struct, 'SWI7 1.6').
runner:case(put_dict, 2, extend_struct, 'SWI7 1.6, XLOG 1') :- true.
