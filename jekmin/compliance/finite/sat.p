/**
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

:- use_module(library(finite/clpb)).
:- use_module(library(misc/residue)).
:- use_module(library(basic/lists)).

% expr_eval/2
runner:ref(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1').
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 1') :-
   catch(sat(7), error(E,_), true),
   E == type_error(sat_expr,7).
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 2') :-
   call_residue(sat(~X+Y), L),
   L == [sat((X->(Y->1;0);1))].
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 3') :-
   call_residue(sat(~(X* ~Y)), L),
   L == [sat((X->(Y->1;0);1))].
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 4') :-
   call_residue(sat(X=<Y), L),
   L == [sat((X->(Y->1;0);1))].
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 5') :-
   call_residue(sat(~(X>Y)), L),
   L == [sat((X->(Y->1;0);1))].
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 6') :-
   call_residue(sat(X#Y), L),
   L == [sat((X->(Y->0;1);Y->1;0))].
runner:case(expr_eval, 2, finite_sat, 'CLP(B) 0.9.4, 1.1, XLOG 7') :-
   call_residue(sat(~(X=:=Y)), L),
   L == [sat((X->(Y->0;1);Y->1;0))].

% sat/1
runner:ref(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2').
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 1') :-
   sat(X+_),
   catch(X = 7, error(E,_), true),
   E == type_error(sat_value,7).
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 2') :-
   sat(~X+X).
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 3') :-
   \+ sat(~X*X).
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 4') :-
   sat(X* ~Y*Z),
   X == 1,
   Y == 0,
   Z == 1.
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 5') :-
   sat(X=:=Y),
   X == Y.
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 6') :-
   call_residue((  sat(X+ ~Y+Z),
                   Y = 1), L),
   L == [sat((X->1;Z->1;0))].
runner:case(sat, 1, finite_sat, 'CLP(B) 0.9.4, 1.2, XLOG 7') :-
   call_residue((  sat(X+ ~Y+Z),
                   X = Z), L),
   L == [sat((Y->(Z->1;0);1))].

% labeling/1
runner:ref(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3').
runner:case(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3, XLOG 1') :-
   catch(labeling(_), error(E,_), true),
   E == instantiation_error.
runner:case(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3, XLOG 2') :-
   catch(labeling(foo), error(E,_), true),
   E == type_error(list,foo).
runner:case(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3, XLOG 3a') :-
   findall(X-Y-Z, (  sat(X=<Y),
                     sat(Y=<Z),
                     sat(Z=<X),
                     labeling([X,Y,Z])), L),
   L = [R|_],
   R == 0-0-0.
runner:case(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3, XLOG 3b') :-
   findall(X-Y-Z, (  sat(X=<Y),
                     sat(Y=<Z),
                     sat(Z=<X),
                     labeling([X,Y,Z])), L),
   L = [_,R|_],
   R == 1-1-1.
runner:case(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3, XLOG 3c') :-
   findall(X-Y-Z, (  sat(X=<Y),
                     sat(Y=<Z),
                     sat(Z=<X),
                     labeling([X,Y,Z])), L),
   L = [_,_].
runner:case(labeling, 1, finite_sat, 'CLP(B) 0.9.4, 1.3, XLOG 4') :-
   sat(X=<Y),
   sat(X=< ~Y),
   sat(~X=<Y),
   sat(~X=< ~Y),
   \+ labeling([X,Y]).

% count/2
runner:ref(count, 2, finite_sat, 'CLP(B) 0.9.4, 1.4').
runner:case(count, 2, finite_sat, 'CLP(B) 0.9.4, 1.4, XLOG 1') :-
   catch(count(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(count, 2, finite_sat, 'CLP(B) 0.9.4, 1.4, XLOG 2') :-
   catch(count(foo, _), error(E,_), true),
   E == type_error(list,foo).
runner:case(count, 2, finite_sat, 'CLP(B) 0.9.4, 1.4, XLOG 3') :-
   sat(X=<Y),
   sat(Y=<Z),
   sat(Z=<X),
   count([X,Y,Z], N),
   N == 2.
runner:case(count, 2, finite_sat, 'CLP(B) 0.9.4, 1.4, XLOG 4') :-
   sat(X=<Y),
   sat(X=< ~Y),
   sat(~X=<Y),
   sat(~X=< ~Y),
   count([X,Y], N),
   N == 0.

% card/2
runner:ref(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5').
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 1') :-
   catch(sat(card(_,_)), error(E,_), true),
   E == instantiation_error.
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 2') :-
   catch(sat(card([3],foo)), error(E,_), true),
   E == type_error(list,foo).
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 3') :-
   length(L, 3),
   \+ sat(card([-1],L)).
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 4') :-
   length(L, 3),
   \+ sat(card([4],L)).
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 5a') :-
   findall(L, (  length(L, 3),
                 sat(card([2],L)),
                 labeling(L)), R),
   R = [S|_],
   S == [0,1,1].
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 5b') :-
   findall(L, (  length(L, 3),
                 sat(card([2],L)),
                 labeling(L)), R),
   R = [_,S|_],
   S == [1,0,1].
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 5c') :-
   findall(L, (  length(L, 3),
                 sat(card([2],L)),
                 labeling(L)), R),
   R = [_,_,S|_],
   S == [1,1,0].
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 5d') :-
   findall(L, (  length(L, 3),
                 sat(card([2],L)),
                 labeling(L)), R),
   R = [_,_,_].
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.5, 1.5, XLOG 6') :-
   length(L, 6),
   sat(card([3],L)),
   count(L, N),
   N == 20.
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.6, 1.5, XLOG 7') :-
   sat(card([2],[X0,X1,X1])),
   X0 == 0,
   X1 == 1.
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.6, 1.5, XLOG 8a') :-
   findall(L, (  L = [X0,X1,Y0,Y1],
                 sat(card([7],[X0,X1,X1,Y0,Y0,Y1,Y1,Y1,Y1])),
                 labeling(L)), R),
   R = [S|_],
   S == [1,0,1,1].
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.6, 1.5, XLOG 8b') :-
   findall(L, (  L = [X0,X1,Y0,Y1],
                 sat(card([7],[X0,X1,X1,Y0,Y0,Y1,Y1,Y1,Y1])),
                 labeling(L)), R),
   R = [_,S|_],
   S == [1,1,0,1].
runner:case(card, 2, finite_sat, 'CLP(B) 0.9.6, 1.5, XLOG 8c') :-
   findall(L, (  L = [X0,X1,Y0,Y1],
                 sat(card([7],[X0,X1,X1,Y0,Y0,Y1,Y1,Y1,Y1])),
                 labeling(L)), R),
   R = [_,_].
