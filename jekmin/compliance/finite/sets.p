/**
 * Prolog code for the sets test cases.
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

:- use_module(library(misc/residue)).
:- use_module(library(advanced/sets)).
:- use_module(library(finite/clpfd)).
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% sys_expr_range(+Integer, +Integer, -Set)
runner:ref(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1').
runner:case(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1, XLOG 1') :-
   X in 1..3,
   var(X).
runner:case(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1, XLOG 2') :-
   X in 1..1,
   X == 1.
runner:case(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1, XLOG 3') :-
   \+ _ in 2..1.
runner:case(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1, XLOG 4') :-
   call_residue(_ in inf..sup, L),
   L == [].
runner:case(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1, XLOG 5') :-
   catch(_ in 1..inf, error(E,_), true),
   E == type_error(fd_set,1..inf).
runner:case(sys_expr_range, 3, finite_sets, 'CLP(FD) 0.8.3, 2.1, XLOG 6') :-
   catch(_ in 1+1, error(E,_), true),
   E == type_error(fd_set,1+1).

% sys_union_set(+Set, +Set, -Set)
runner:ref(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2').
runner:case(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2, XLOG 1') :-
   call_residue(X in 5\/3\/1, L),
   L == [X in 1\/3\/5].
runner:case(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2, XLOG 2') :-
   call_residue(X in 3..sup\/inf..0, L),
   L == [X in inf..0\/3..sup].
runner:case(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2, XLOG 3') :-
   call_residue(X in 1\/2\/3, L),
   L == [X in 1..3].
runner:case(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2, XLOG 4') :-
   call_residue(X in inf..1\/3..sup, L),
   L == [X#\=2].
runner:case(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2, XLOG 5') :-
   call_residue(_ in inf..1\/2..sup, L),
   L == [].
runner:case(sys_union_set, 3, finite_sets, 'CLP(FD) 0.8.3, 2.2, XLOG 6') :-
   catch(_ in _\/5..10, error(E,_), true),
   E == instantiation_error.

% sys_inter_set(+Set, +Set, -Set)
runner:ref(sys_inter_set, 3, finite_sets, 'CLP(FD) 0.8.4, 2.3').
runner:case(sys_inter_set, 3, finite_sets, 'CLP(FD) 0.8.4, 2.3, XLOG 1') :-
   call_residue(X in 0..10/\5..15, L),
   L == [X in 5..10].
runner:case(sys_inter_set, 3, finite_sets, 'CLP(FD) 0.8.4, 2.3, XLOG 2') :-
   \+ _ in 0..5/\10..15.
runner:case(sys_inter_set, 3, finite_sets, 'CLP(FD) 0.8.4, 2.3, XLOG 3') :-
   catch(_ in 5..10/\_..2, error(E,_), true),
   E == instantiation_error.

% sys_comp_set(+Set, -Set)
runner:ref(sys_comp_set, 2, finite_sets, 'CLP(FD) 0.8.4, 2.4').
runner:case(sys_comp_set, 2, finite_sets, 'CLP(FD) 0.8.4, 2.4, XLOG 1') :-
   call_residue(X in \5..10, L),
   L == [X in inf..4\/11..sup].
runner:case(sys_comp_set, 2, finite_sets, 'CLP(FD) 0.8.4, 2.4, XLOG 2') :-
   call_residue(_ in 1\/ \1, L),
   L == [].
runner:case(sys_comp_set, 2, finite_sets, 'CLP(FD) 0.8.4, 2.4, XLOG 3') :-
   catch(_ in \0.._, error(E,_), true),
   E == instantiation_error.

% sys_compare_expr(+Expr, +Expr, +Expr)
runner:ref(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5').
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 1') :-
   X #\= _,
   var(X).
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 2') :-
   call_residue(3*X #< 2*Y+X, L),
   L == [X#=<Y-1].
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 3') :-
   call_residue(2*Y+X #> 3*X, L),
   L == [Y#>X].
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 4') :-
   call_residue(X*X+Y*Y #=< 100, L),
   L = [_#=W,_,_#=V],
   L == [X*X#=W,V#=< -W+100,Y*Y#=V].
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 5') :-
   call_residue(abs(X)+abs(Y) #>= 100, L),
   L = [_#=W,_,_#=V],
   L == [abs(X)#=W,V#> -W+99,abs(Y)#=V].
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 6') :-
   call_residue(3*_ #\= 100, L),
   L == [].
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 7') :-
   call_residue(100*X #> 100, L),
   L == [X#>1].
runner:case(sys_compare_expr, 3, finite_sets, 'CLP(FD) 0.8.3, 2.5, XLOG 8') :-
   call_residue(101*X #< 100, L),
   L == [X#=<0].

% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
runner:ref(sys_set_agent, 5, finite_sets, 'CLP(FD) 0.8.4, 2.6').
runner:case(sys_set_agent, 5, finite_sets, 'CLP(FD) 0.8.4, 2.6, XLOG 1') :-
   \+ _+_ in \1/\1.
runner:case(sys_set_agent, 5, finite_sets, 'CLP(FD) 0.8.4, 2.6, XLOG 2') :-
   \+ 2*_+2*_ in 3\/5\/7.
runner:case(sys_set_agent, 5, finite_sets, 'CLP(FD) 0.8.4, 2.6, XLOG 3') :-
   call_residue(6*X+15*Y in 10..20, L),
   L == [2*X+5*Y in 4..6].

% sys_const(+Wrap, +Integer)
% sys_in(+Wrap, +Set, +Bound)
runner:ref('sys_const sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.7').
runner:case('sys_const sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.7, XLOG 1') :-
   X in 1..100,
   X = 30.
runner:case('sys_const sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.7, XLOG 2') :-
   \+ (  X in 1..100,
         X = 130).
runner:case('sys_const sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.7, XLOG 3') :-
   catch((  X in 1..100,
            X = a), error(E,_), true),
   E == type_error(integer,a).

% sys_var(+Wrap, +Wrap)
% sys_in(+Wrap, +Set, +Bound)
runner:ref('sys_var sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.8').
runner:case('sys_var sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.8, XLOG 1') :-
   call_residue((  Y+Z #= X,
                   X in 1..100,
                   Y #= 0), L),
   L == [Z in 1..100].
runner:case('sys_var sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.8, XLOG 2') :-
   -X in 5,
   X == -5.
runner:case('sys_var sys_in', 5, finite_sets, 'CLP(FD) 0.8.3, 2.8, XLOG 3') :-
   \+ 2*_ in 5.

% sys_in(+Wrap, +Set, +Bound)
% sys_in(+Wrap, +Set, +Bound)
runner:ref('sys_in sys_in', 6, finite_sets, 'CLP(FD) 0.8.3, 2.9').
runner:case('sys_in sys_in', 6, finite_sets, 'CLP(FD) 0.8.3, 2.9, XLOG 1') :-
   call_residue((  X in 1..100,
                   X in 50..150), L),
   L == [X in 50..100].
runner:case('sys_in sys_in', 6, finite_sets, 'CLP(FD) 0.8.3, 2.9, XLOG 2') :-
   \+ (  X in 1..50,
         X in 100..150).
runner:case('sys_in sys_in', 6, finite_sets, 'CLP(FD) 0.8.3, 2.9, XLOG 3') :-
   X in 3..5,
   X in 1..3,
   X == 3.
runner:case('sys_in sys_in', 6, finite_sets, 'CLP(FD) 0.8.3, 2.9, XLOG 4') :-
   call_residue((  X in 0..5,
                   X in 3\/5\/7), L),
   L == [X in 3\/5].
runner:case('sys_in sys_in', 6, finite_sets, 'CLP(FD) 0.8.3, 2.9, XLOG 5') :-
   call_residue((  X in \ (3\/5)\/7,
                   X in 0..5), L),
   L == [X in 0..2\/4].

% sys_const(+Wrap, +Integer)
% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
runner:ref('sys_const sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.10').
runner:case('sys_const sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.10, XLOG 1') :-
   call_residue((  X #=< 3*Z-2*Y,
                   X = 1), L),
   L == [2*Y#=<3*Z-1].
runner:case('sys_const sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.10, XLOG 2') :-
   call_residue((  X #> 3*Z-2*Y,
                   Y = 2), L),
   L == [X#>3*Z-4].
runner:case('sys_const sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.10, XLOG 3') :-
   call_residue((  X #\= -3*Z+2*Y,
                   Z = 3), L),
   L == [X#\=2*Y-9].
runner:case('sys_const sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.10, XLOG 4') :-
   catch((  _+X in 1..100,
            X = a), error(E,_), true),
   E == type_error(integer,a).

% sys_var(+Wrap, +Wrap)
% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
runner:ref('sys_var sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.11').
runner:case('sys_var sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.11, XLOG 1') :-
   call_residue((  X #=< 3*Z-2*Y,
                   X = Y), L),
   L == [X#=<Z].
runner:case('sys_var sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.11, XLOG 2') :-
   call_residue((  X #> 3*Z-2*Y,
                   X = Z), L),
   L == [Y#>X].
runner:case('sys_var sys_set', 7, finite_sets, 'CLP(FD) 0.8.3, 2.11, XLOG 3') :-
   call_residue((  X #\= 3*Z-2*Y,
                   Z = Y), L),
   L == [X#\=Z].

% sys_in(+Wrap, +Set, +Bound)
% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
runner:ref('sys_in sys_set', 8, finite_sets, 'CLP(FD) 0.8.3, 2.12').
runner:case('sys_in sys_set', 8, finite_sets, 'CLP(FD) 0.8.3, 2.12, XLOG 1') :-
   call_residue((  X+Y in 1..100,
                   X in 1..100), L),
   permutation(L, [Y in-99..99,X in 1..100,X+Y in 1..100]).
runner:case('sys_in sys_set', 8, finite_sets, 'CLP(FD) 0.8.3, 2.12, XLOG 2') :-
   call_residue((  X+Y in 1..100,
                   Y in 1..100), L),
   L == [Y in 1..100,X+Y in 1..100].
runner:case('sys_in sys_set', 8, finite_sets, 'CLP(FD) 0.8.3, 2.12, XLOG 3') :-
   call_residue((  3*X+Y in 1..100,
                   X in 1..100), L),
   permutation(L, [Y in-299..97,X in 1..100,3*X+Y in 1..100]).
runner:case('sys_in sys_set', 8, finite_sets, 'CLP(FD) 0.8.3, 2.12, XLOG 4') :-
   call_residue((  X+3*Y in 1..100,
                   X in 1..100), L),
   permutation(L, [Y in-33..33,X in 1..100,X+3*Y in 1..100]).
runner:case('sys_in sys_set', 8, finite_sets, 'CLP(FD) 0.8.3, 2.12, XLOG 5') :-
   call_residue((  3*X+2*Y in 1..100,
                   X in 1..100), L),
   permutation(L, [Y in-149..48,X in 1..100,3*X+2*Y in 1..100]).

% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
runner:ref('sys_set sys_set', 10, finite_sets, 'CLP(FD) 0.8.3, 2.13').
runner:case('sys_set sys_set', 10, finite_sets, 'CLP(FD) 0.8.3, 2.13, XLOG 1') :-
   call_residue((  X+Y in 1..100,
                   X+Y in 50..150), L),
   L == [X+Y in 50..100].
runner:case('sys_set sys_set', 10, finite_sets, 'CLP(FD) 0.8.3, 2.13, XLOG 2') :-
   \+ (  X+Y in 1..50,
         X+Y in 100..150).
runner:case('sys_set sys_set', 10, finite_sets, 'CLP(FD) 0.8.3, 2.13, XLOG 3') :-
   call_residue((  X+Y in 3..5,
                   X+Y in 1..3), L),
   L == [Y#= -X+3].

% +List ins +Set
runner:ref(ins, 2, finite_sets, 'CLP(FD) 0.8.4, 2.14').
runner:case(ins, 2, finite_sets, 'CLP(FD) 0.8.4, 2.14, XLOG 1') :-
   catch([_|_] ins 10..15, error(E,_), true),
   E == instantiation_error.
runner:case(ins, 2, finite_sets, 'CLP(FD) 0.8.4, 2.14, XLOG 2') :-
   call_residue([X,Y] ins 1\/2\/3, L),
   L == [X in 1..3,Y in 1..3].
runner:case(ins, 2, finite_sets, 'CLP(FD) 0.8.4, 2.14, XLOG 3') :-
   catch(foo ins 10..sup, error(E,_), true),
   E == type_error(list,foo).

% all_different(+List)
runner:ref(all_different, 1, finite_sets, 'CLP(FD) 0.8.3, 2.15').
runner:case(all_different, 1, finite_sets, 'CLP(FD) 0.8.3, 2.15, XLOG 1') :-
   all_different([X,_,_]),
   var(X).
runner:case(all_different, 1, finite_sets, 'CLP(FD) 0.8.3, 2.15, XLOG 2') :-
   \+ (  all_different([X,Y,_]),
         X = Y).
runner:case(all_different, 1, finite_sets, 'CLP(FD) 0.8.3, 2.15, XLOG 3') :-
   all_different([X,Y,_]),
   X = 1,
   Y = 2.
runner:case(all_different, 1, finite_sets, 'CLP(FD) 0.8.3, 2.15, XLOG 4') :-
   catch(all_different(_), error(E,_), true),
   E == instantiation_error.
runner:case(all_different, 1, finite_sets, 'CLP(FD) 0.8.3, 2.15, XLOG 5') :-
   catch(all_different([_|foo]), error(E,_), true),
   E == type_error(list,foo).
