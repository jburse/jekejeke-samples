/**
 * Prolog code for the functions on dicts test cases.
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

:- use_module(library(advanced/dict)).
:- use_module(library(advanced/func)).

runner:ref(field_access, 2, extend_invoke, 'SWI7 1.1').
runner:case(field_access, 2, extend_invoke, 'SWI7 1.1, XLOG 1') :-
   P = point{x:1,y:2},
   X = P.x,
   Y = P.y,
   X == 1,
   Y == 2.
runner:case(field_access, 2, extend_invoke, 'SWI7 1.1, XLOG 2') :-
   findall(K-V, (  P = point{x:1,y:2},
                   V = P.K), [J-W|_]),
   J == x,
   W == 1.
runner:case(field_access, 2, extend_invoke, 'SWI7 1.1, XLOG 3') :-
   findall(K-V, (  P = point{x:1,y:2},
                   V = P.K), [_,J-W|_]),
   J == y,
   W == 2.
runner:case(field_access, 2, extend_invoke, 'SWI7 1.1, XLOG 4') :-
   findall(K-V, (  P = point{x:1,y:2},
                   V = P.K), [_,_]).
runner:case(field_access, 2, extend_invoke, 'SWI7 1.1, XLOG 5') :-
   B = book{author:person{name:bob}},
   N = B.author.name,
   N == bob.
runner:case(field_access, 2, extend_invoke, 'SWI7 1.1, XLOG 6') :-
   catch((  P = point{x:1,y:2},
            _ = P.z), error(E,_), true),
   E = existence_error(key,z).

Pt.offset(Dx,Dy) := point{x:X,y:Y} :-
   X is Pt.x+Dx,
   Y is Pt.y+Dy.

runner:ref(func_call, 2, extend_invoke, 'SWI7 1.2').
runner:case(func_call, 2, extend_invoke, 'SWI7 1.1, XLOG 1') :-
   P = point{x:1,y:2},
   Q = P.offset(3,4),
   Q == point{x:4,y:6}.
runner:case(func_call, 2, extend_invoke, 'SWI7 1.1, XLOG 2') :-
   catch((  P = point{x:1,y:2},
            _ = P.foo(77)), error(E,_), true),
   nonvar(E),
   E = existence_error(procedure,_).
