/**
 * Prolog code for the linear test cases.
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
:- use_module(library(experiment/sets)).
:- use_module(library(finite/clpfd)).
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% sys_add_prod(+Prod, +Prod, -Prod)
runner:ref(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1').
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 1') :-
   _ #= 2*Y, var(Y).
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 2') :-
   call_residue(0-X #= Y, L), L == [X #= -Y].
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 3') :-
   X+0 #= Y, X == Y.
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 4') :-
   call_residue(2*X+X*3 #= Y, L), L == [5*X #= Y].
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 5') :-
   3*X-Y #= 2*X, X == Y.
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 6') :-
   call_residue(X*2+3*Y+X #= Z, L), L == [3*Y #= Z-3*X].
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 7') :-
   call_residue(2*X-Y+X*3 #= Z, L), L == [Y #= -Z+5*X].
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 8') :-
   catch((X+_ #= 100, X = a), error(E, _), true),
   E == type_error(integer, a).
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 9') :-
   catch(1.2+_ #= _, error(E, _), true),
   E == type_error(fd_value, 1.2).
runner:case(sys_add_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.1, XLOG 10') :-
   catch(_ #= 1.2+_, error(E, _), true),
   E == type_error(fd_value, 1.2).

% sys_mul_prod(+Prod, +Integer, -Prod)
runner:ref(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2').
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 1') :-
   call_residue(Z #= 2*(X+Y), L), L == [Z #= 2*X+2*Y].
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 2') :-
   call_residue(2*Z #= 2*(X+Y), L), L == [Z #= X+Y].
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 3') :-
   call_residue(0 #= 6*(X-Y)-3*X, L), L == [2*Y #= X].
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 4') :-
   X*Y #= Z, X = Y, Z = 0, X == 0.
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 5') :-
   X = Y, X*Y #= Z, Z = 0, X == 0.
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 6') :-
   Z = 0, X*Y #= Z, X = Y, X == 0.
runner:case(sys_mul_prod, 3, finite_linear, 'CLP(FD) 0.8.3, 1.2, XLOG 7') :-
   catch((X*_ #= 100, X = a), error(E, _), true),
   E == type_error(integer, a).

% sys_const(+Wrap, +Integer)
% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
runner:ref('sys_const sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.3').
runner:case('sys_const sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.3, XLOG 1') :-
   call_residue((X #= Y+2*Z-3*T, Y = 1), L), L == [X #= 2*Z-3*T+1].
runner:case('sys_const sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.3, XLOG 2') :-
   call_residue((X #= Y+2*Z-3*T, Z = 2), L), L == [X #= Y-3*T+4].
runner:case('sys_const sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.3, XLOG 3') :-
   call_residue((X #= Y+2*Z-3*T, T = 3), L), L == [X #= Y+2*Z-9].

% sys_var(+Wrap, +Wrap)
% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
runner:ref('sys_var sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.4').
runner:case('sys_var sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.4, XLOG 1') :-
   call_residue((X #= Y+2*Z-3*T, Y = X), L), L == [3*T #= 2*Z].
runner:case('sys_var sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.4, XLOG 2') :-
   call_residue((X #= Y+2*Z-3*T, Z = X), L), L == [X #= -Y+3*T].
runner:case('sys_var sys_lin', 6, finite_linear, 'CLP(FD) 0.8.3, 1.4, XLOG 3') :-
   call_residue((X #= Y+2*Z-3*T, T = X), L), L == [4*X #= Y+2*Z].

% sys_in(+Wrap, +Set, +Bound)
% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
runner:ref('sys_in sys_lin', 7, finite_linear, 'CLP(FD) 0.8.3, 1.5').
runner:case('sys_in sys_lin', 7, finite_linear, 'CLP(FD) 0.8.3, 1.5, XLOG 1') :-
   call_residue((X+Y #= 50, X in 1..100), L),
   eq_equal(L, [Y in -50..49, X in 1..100, Y #= -X+50]).
runner:case('sys_in sys_lin', 7, finite_linear, 'CLP(FD) 0.8.3, 1.5, XLOG 2') :-
   call_residue((X+Y #= 50, Y in 1..100), L),
   eq_equal(L, [Y in 1..100, Y #= -X+50]).
runner:case('sys_in sys_lin', 7, finite_linear, 'CLP(FD) 0.8.3, 1.5, XLOG 3') :-
   call_residue((X in 1..100, 3*X+Y #= 50), L),
   eq_equal(L, [Y in -250..47, X in 1..100, Y #= - (3*X)+50]).
runner:case('sys_in sys_lin', 7, finite_linear, 'CLP(FD) 0.8.3, 1.5, XLOG 4') :-
   call_residue((X+2*Y #= 50, X in 1..100), L),
   eq_equal(L, [Y in -25..24, X in 1..100, 2*Y #= -X+50]).
runner:case('sys_in sys_lin', 7, finite_linear, 'CLP(FD) 0.8.3, 1.5, XLOG 5') :-
   call_residue((X in 1..100, 3*X+2*Y #= 50), L),
   eq_equal(L, [Y in -125..23, X in 1..100, 2*Y #= - (3*X)+50]).

% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
runner:ref('sys_lin sys_lin', 8, finite_linear, 'CLP(FD) 0.8.3, 1.6').
runner:case('sys_lin sys_lin', 8, finite_linear, 'CLP(FD) 0.8.3, 1.6, XLOG 1') :-
   call_residue((X+Y #= 10, X+Y #= 10), L),
   L == [Y #= -X+10].
runner:case('sys_lin sys_lin', 8, finite_linear, 'CLP(FD) 0.8.3, 1.6, XLOG 2') :-
   \+ (X+Y #= 10, X+Y #= 20).
runner:case('sys_lin sys_lin', 8, finite_linear, 'CLP(FD) 0.8.3, 1.6, XLOG 3') :-
   call_residue((X+Y #= Z, X+Y #= T), L), L == [Y #= Z-X, T #= X+Y].

% sys_mul_lin(+Prod, +Integer, +Prod, +Integer, -Prod, -Integer, +Wrap)
runner:ref(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7').
runner:case(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7, XLOG 1') :-
   X*_ #= X, var(X).
runner:case(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7, XLOG 2') :-
   call_residue(Z #= 2*(X+Y), L), L == [Z #= 2*X+2*Y].
runner:case(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7, XLOG 3') :-
   call_residue(Z #= X*(X+Y), L), L = [V #= _, _],
   L == [V #= X+Y, Z #= X*V].
runner:case(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7, XLOG 4') :-
   call_residue((X+Y)*2 #= Z, L), L == [2*Y #= Z-2*X].
runner:case(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7, XLOG 5') :-
   X*Y #= 2100, X+Y #= 100, Y = 30, X == 70.
runner:case(sys_mul_lin, 7, finite_linear, 'CLP(FD) 0.8.3, 1.7, XLOG 6') :-
   X+Y #= 100, X*Y #= 2100, Y = 30, X == 70.

% sys_const(+Wrap, +Integer)
% sys_mulv(+Wrap, +Wrap, +Wrap)
runner:ref('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8').
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 1') :-
   call_residue((X*Y #= Z, Y = 2), L), L == [2*X #= Z].
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 2') :-
   call_residue((Z #= X*Y, Z = 12), L), eq_equal(L, [Y in -12..12, 12 #= X*Y]).
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 3') :-
   X*Y #= 12, X = 3, Y == 4.
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 4') :-
   \+ (X*_ #= 12, X = 5).
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 5') :-
   call_residue((X*X #= Y, Y = 16), L), L == [X in -4\/4].
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 6') :-
   \+ (X*X #= Y, Y = 15).
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 7') :-
   X*Y #= X, Y = 5, X == 0.
runner:case('sys_const sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.8, XLOG 8') :-
   X*Y #= X, X = 5, Y == 1.

% sys_var(+Wrap, +Wrap)
% sys_mulv(+Wrap, +Wrap, +Wrap)
runner:ref('sys_var sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.9').
runner:case('sys_var sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.9, XLOG 1') :-
   call_residue((X*Y #= Z, Y = Z), L), L == [Y #= Y*X].
runner:case('sys_var sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.9, XLOG 2') :-
   call_residue((X*Y #= Z, X = Y), L), L == [X*X #= Z].
runner:case('sys_var sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.9, XLOG 3') :-
   call_residue((X*Y #= Z, X = Y), L), L == [X*X #= Z].
runner:case('sys_var sys_mulv', 5, finite_linear, 'CLP(FD) 0.8.3, 1.9, XLOG 4') :-
   call_residue((X*Y #= X, X = Y), L), L == [X in 0..1].

% sys_in(+Wrap, +Set, +Bound)
% sys_mulv(+Wrap, +Wrap, +Wrap)
% SWI-Prolog usually has stronger bounds
runner:ref('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10').
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 1') :-
   call_residue((X in 3..6, Y in 4..8, Z #= X*Y), L),
   eq_equal(L, [Z in 12..48, X in 3..6, Y in 4..8, Z #= X*Y]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 2') :-
   call_residue((Z #= X*Y, X in 3..6, Y in 4..8), L),
   eq_equal(L, [Y in 4..8, Z in 12..48, X in 3..6, Z #= X*Y]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 3') :-
   call_residue((X*Y #= Z, X in 3..6, Z in 12..48), L),
   eq_equal(L, [Z in 12..48, X in 3..6, Y in 2..16, X*Y #= Z]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 4') :-
   call_residue((X in 3..6, Z in 12..48, X*Y #= Z), L),
   eq_equal(L, [Y in 2..16, X in 3..6, Z in 12..48, X*Y #= Z]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 5') :-
   call_residue((X*Y #= 100, X in 1..20), L),
   eq_equal(L, [Y in 5..100, X in 1..20, 100 #= X*Y]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 6') :-
   call_residue((X in 1..20, X*Y #= 100), L),
   eq_equal(L, [Y in 5..100, X in 1..20, 100 #= X*Y]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 7') :-
   call_residue((Y #= X*X, X in 5..10), L),
   eq_equal(L, [X in 5..10, Y in 25..100, Y #= X*X]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 8') :-
   call_residue((X in 5..10, Y #= X*X), L),
   eq_equal(L, [Y in 25..100, X in 5..10, Y #= X*X]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 9') :-
   call_residue((X*X #= Y, Y in 20..80), L),
   eq_equal(L, [Y in 20..80, X in -8..8, X*X #= Y]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 10') :-
   call_residue((Y in 20..80, X*X #= Y), L),
   eq_equal(L, [X in -8..8, Y in 20..80, X*X #= Y]).
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 11') :-
   X*Y #= X, X in 10..20, Y == 1.
runner:case('sys_in sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.10, XLOG 12') :-
   X in 10..20, X*Y #= X, Y == 1.

% sys_mulv(+Wrap, +Wrap, +Wrap)
% sys_mulv(+Wrap, +Wrap, +Wrap)
% Not implemented by SWI-Prolog
runner:ref('sys_mulv sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.11').
runner:case('sys_mulv sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.11, XLOG 1') :-
   X*Y #= Z, X*Y #= T, Z == T.
runner:case('sys_mulv sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.11, XLOG 2') :-
   X*Y #= Z, X*Y #= 2, Z == 2.
runner:case('sys_mulv sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.11, XLOG 3') :-
   X*Y #= 2, X*Y #= 2.
runner:case('sys_mulv sys_mulv', 6, finite_linear, 'CLP(FD) 0.8.3, 1.11, XLOG 4') :-
   \+ (X*Y #= 2, X*Y #= 3).

% Array subscripts
% Not implemented by SWI-Prolog
/* right hand side */
/*
runner:ref('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12').
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 1') :-
   X = #(1,2,3), Y #= X[1]+X[2]+X[3], Y == 6.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 2') :-
   X = #(#(1,2,3),#(4,5,6),#(7,8,9)), Y #= X[1,3]+X[2,3]+X[3,3], Y == 18.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 3') :-
   X = #(#(#(_,A,_))), Y #= X[1,1,2], Y == A.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 4') :-
   X = #(#(#(#(_,_,_),#(_,_,A),#(_,_,_)))), Y #= X[1,1,2,3], Y == A.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 5') :-
   X = #(#(#(#(#(A,_,B))))), call_residue((Y #= X[1,1,1,1,1]+X[1,1,1,1,3]),L),
   L == [Y#=A+B].
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 6') :-
   X = #(#(#(#(#(#(_,_,A),#(_,_,_),#(_,_,B)))))),
   call_residue((Y #= X[1,1,1,1,1,3]+X[1,1,1,1,3,3]),L),
   L == [Y#=A+B].
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 7') :-
   X = #(#(#(#(#(#(#(A,_,1))))))), call_residue((Y #= X[1,1,1,1,1,1,1]+X[1,1,1,1,1,1,3]),L),
   L == [Y#=A+1].
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 8') :-
   catch((X = #(1,2,3), _ #= X[0]), error(E, _), true),
   E == evaluation_error(array_index).
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 9') :-
   catch((X = #(#(1,2,3),#(4,5,6),#(7,8,9)), _ #= X[1,4]), error(E, _), true),
   E == evaluation_error(array_index).
*/

/* left hand side */
/*
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 10') :-
   X = #(1,2,3), X[1]+X[2]+X[3] #= Y, Y == 6.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 11') :-
   X = #(#(1,2,3),#(4,5,6),#(7,8,9)), X[1,3]+X[2,3]+X[3,3] #= Y, Y == 18.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 12') :-
   X = #(#(#(_,A,_))), X[1,1,2] #= Y, Y == A.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 13') :-
   X = #(#(#(#(_,_,_),#(_,_,A),#(_,_,_)))), X[1,1,2,3] #= Y, Y == A.
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 14') :-
   X = #(#(#(#(#(A,_,B))))), call_residue((X[1,1,1,1,1]+X[1,1,1,1,3] #= Y),L),
   L == [B#=Y-A].
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 15') :-
   X = #(#(#(#(#(#(_,_,A),#(_,_,_),#(_,_,B)))))),
   call_residue((X[1,1,1,1,1,3]+X[1,1,1,1,3,3] #= Y),L),
   L == [B#=Y-A].
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 16') :-
   X = #(#(#(#(#(#(#(A,_,1))))))), call_residue((X[1,1,1,1,1,1,1]+X[1,1,1,1,1,1,3] #= Y),L),
   L == [A#=Y-1].
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 17') :-
   catch((X = #(1,2,3), X[0] #= _), error(E, _), true),
   E == evaluation_error(array_index).
runner:case('array_subs', 0, finite_linear, 'CLP(FD) 0.8.8, 1.12, XLOG 18') :-
   catch((X = #(#(1,2,3),#(4,5,6),#(7,8,9)), X[1,4] #= _), error(E, _), true),
   E == evaluation_error(array_index).
*/
