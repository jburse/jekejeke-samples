/**
 * Prolog code for the guard test cases.
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
:- use_module(library(finite/clpfd)).
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% sys_reify_in(+Expr, +Set, +Var)
runner:ref(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1').
runner:case(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1, XLOG 1') :-
   call_residue((X in 10..20 #<==> B, B = 0), L),
   L == [X in inf..9\/21..sup].
runner:case(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1, XLOG 2') :-
   call_residue((X in 10..20 #<==> B, B = 1), L),
   L == [X in 10..20].
runner:case(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1, XLOG 3') :-
   X in 10..20 #<==> B, X = 15, B == 1.
runner:case(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1, XLOG 4') :-
   X in 10..20 #<==> B, X = 25, B == 0.
runner:case(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1, XLOG 5') :-
   call_residue(#\ X in 10..20, L),
   L == [X in inf..9\/21..sup].
runner:case(sys_reify_in, 3, finite_guard, 'CLP(FD) 0.8.3, 5.1, XLOG 6') :-
   catch((X in 10..20 #<==> _, X = a), error(E, _), true),
   E == type_error(integer, a).

% sys_reify_lin(+Expr, +Expr, +Var)
runner:ref(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2').
runner:case(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2, XLOG 1') :-
   call_residue((X+Y #= 100 #<==> B, B = 0), L),
   L == [Y #\= -X+100].
runner:case(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2, XLOG 2') :-
   call_residue((X+Y #= 100 #<==> B, B = 1), L),
   L == [Y #= -X+100].
runner:case(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2, XLOG 3') :-
   X+Y #= 100 #<==> B, Y = 70, X = 30, B == 1.
runner:case(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2, XLOG 4') :-
   X+Y #= 100 #<==> B, X = 75, Y = 36, B == 0.
runner:case(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2, XLOG 5') :-
   call_residue(#\ X+Y #= 100, L),
   L == [Y #\= -X+100].
runner:case(sys_reify_lin, 3, finite_guard, 'CLP(FD) 0.8.3, 5.2, XLOG 6') :-
   catch((_+Y #= 100 #<==> _, Y = a), error(E, _), true),
   E == type_error(integer, a).

% sys_reify_set(+Expr, +Expr, +Set, +Var)
runner:ref(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3').
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 1') :-
   call_residue((X #< 100-Y #<==> B, B = 0), L),
   L == [X #> -Y+99].
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 2') :-
   call_residue((X #< 100-Y #<==> B, B = 1), L),
   L == [X #=< -Y+99].
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 3') :-
   X #< 100-Y #<==> B, Y = 74, X = 25, B == 1.
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 4') :-
   99-Y #>= X #<==> B, X = 30, Y = 70, B == 0.
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 5') :-
   call_residue(#\ X #< 100-Y, L),
   L == [X #> -Y+99].
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 6') :-
   catch((X #< 100-_ #<==> _, X = a), error(E, _), true),
   E == type_error(integer, a).
runner:case(sys_reify_set, 4, finite_guard, 'CLP(FD) 0.8.3, 5.3, XLOG 7') :-
   2*_-1 #\= 0 #<==> B, B == 1.

% sys_in(+Wrap, +Set, +Bound)
% sys_reify_in(+Expr, +Set, +Var)
runner:ref('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4').
/* at & at */
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 1') :-
   call_residue((X in 10..20 #<==> B, X in 5..15 #<==> B), L),
   L == [X in 10..15 #\/ B #= 0, X in inf..4\/21..sup #\/ B #= 1, B in 0..1].
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 2') :-
   call_residue((X in 10..20 #<==> B, X #= 15 #<==> B), L),
   L == [X #= 15 #\/ B #= 0, X in inf..9\/21..sup #\/ B #= 1, B in 0..1].
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 3') :-
   call_residue((X #\= 15 #<==> B, X in 10..20 #<==> B), L),
   L == [X in 10..14\/16..20], B == 1.
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 4') :-
   call_residue((X in 10..20 #<==> B, X #> 4 #<==> B), L),
   L == [X #=< 20, X #=< 4 #\/ B #= 1, X in 10..20 #\/ B #= 0, B in 0..1].
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 5') :-
   call_residue((X #=< 20 #<==> B, X in 5..15 #<==> B), L),
   L == [X #> 4, X in 5..15 #\/ B #= 0, X #> 20 #\/ B #= 1, B in 0..1].
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 6') :-
   call_residue((X #= 3 #<==> B, X #= 7 #<==> B), L),
   L == [X in inf..2\/4..6\/8..sup], B == 0.
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 7') :-
   call_residue((X #= 3 #<==> B, X #\= 7 #<==> B), L),
   L == [X in 3..7, X #= 7 #\/ B #= 1, X #= 3 #\/ B #= 0, B in 0..1].
/* in & at */
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 8') :-
   call_residue((X in 10..20 #<==> B, X in 5..15), L),
   L == [X in 5..15, X in 10..15 #\/ B #= 0, X in 5..9 #\/ B #= 1, B in 0..1].
runner:case('sys_in sys_reify_in', 6, finite_guard, 'CLP(FD) 0.8.4, 5.4, XLOG 9') :-
   call_residue((X in 5..15, X in 10..20 #<==> B), L),
   L == [X in 5..15, X in 10..15 #\/ B #= 0, X in 5..9 #\/ B #= 1, B in 0..1].

% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
% sys_reify_lin(+Expr, +Expr, +Var)
runner:ref('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5').
/* Pit & Pit */
runner:case('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5, XLOG 1') :-
   call_residue((X+Y #= 3 #<==> B, X+Y #= 7 #<==> B), L),
   L == [X+Y in inf..2\/4..6\/8..sup], B == 0.
runner:case('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5, XLOG 2') :-
   call_residue((X-Y #= 3 #<==> B, X-Y #\= 7 #<==> B), L),
   L == [Y #= X-3 #\/ B #= 0, Y #= X-7 #\/ B #= 1, B in 0..1].
/* Pit & Set */
runner:case('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5, XLOG 3') :-
   X+Y #= 3 #<==> B, X+Y in 10..15, B == 0.
runner:case('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5, XLOG 4') :-
   call_residue((X+Y in 0..5, X+Y #= 3 #<==> B), L),
   L == [X+Y in 0..5, X+Y in 0..2\/4..5 #\/ B #= 1, B in 0..1].
/* Pit & Lin */
runner:case('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5, XLOG 5') :-
   call_residue((X+Y #= 3 #<==> B, X+Y #= 7), L),
   L == [Y #= -X+7], B == 0.
runner:case('sys_lin sys_reify_lin', 7, finite_guard, 'CLP(FD) 0.8.4, 5.5, XLOG 6') :-
   call_residue((X-Y #= 3, X-Y #= 3 #<==> B), L),
   L == [Y #= X-3], B == 1.

% sys_set_agent(+Ref, +Wrap, +Prod, +Set, +Bound)
% sys_reify_set(+Expr, +Expr, +Set, +Var)
runner:ref('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6').
/* Lot & Lot */
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 1') :-
   call_residue((X+Y in 10..20 #<==> B, X+Y in 5..15 #<==> B), L),
   L == [X+Y in 10..15 #\/ B #= 0, X+Y in inf..4\/21..sup #\/ B #= 1, B in 0..1].
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 2') :-
   call_residue((3*X+2*Y in 10..20 #<==> B, 3*X+2*Y #> 4 #<==> B), L),
   L == [3*X+2*Y in 10..20 #\/ B #= 0, 2*Y #=< - (3*X)+4 #\/ B #= 1, B in 0..1].
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 3') :-
   call_residue((X+Y #=< 20 #<==> B, X+Y in 5..15 #<==> B), L),
   L == [X+Y in 5..15 #\/ B #= 0, Y #> -X+20 #\/ B #= 1, B in 0..1].
/* Lot & Set */
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 4') :-
   X+Y #=< 20 #<==> B, X+Y in 10..15, B == 1.
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 5') :-
   call_residue((X+Y #=< 20 #<==> B, X+Y in 20..25), L),
   L == [X+Y in 20..25, X+Y in 21..25 #\/ B #= 1, B in 0..1].
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 6') :-
   call_residue((X+Y in 0..10 #<==> B, X+Y in 5..15), L),
   L == [X+Y in 5..15, X+Y in 5..10 #\/ B #= 0, X+Y in 11..15 #\/ B #= 1, B in 0..1].
/* Lot & Pit */
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 7') :-
   call_residue((X+Y+Z in 10..20 #<==> B, X+Y+Z #= 15 #<==> B), L),
   L == [Z #= -X-Y+15 #\/ B #= 0, X+Y+Z in inf..9\/21..sup #\/ B #= 1, B in 0..1].
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 8') :-
   call_residue((X+Y #\= 15 #<==> B, X+Y in 10..20 #<==> B), L),
   L == [X+Y in 10..14\/16..20], B == 1.
/* Lot & Lin */
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 9') :-
   call_residue((X+Y in 10..20 #<==> B, X+Y #= 15), L),
   L == [Y #= -X+15], B == 1.
runner:case('sys_set sys_reify_set', 9, finite_guard, 'CLP(FD) 0.8.4, 5.6, XLOG 10') :-
   call_residue((X-Y #=< 20 #<==> B, X-Y #= 25), L),
   L == [Y #= X-25], B == 0.
