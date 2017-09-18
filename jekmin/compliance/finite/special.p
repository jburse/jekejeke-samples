/**
 * Prolog code for the special test cases.
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

:- use_module(library(misc/residue)).
:- use_module(library(finite/clpfd)).
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% sys_abs_lin(+Prod, +Integer, -Prod, -Integer, +Wrap)
runner:ref(sys_abs_lin, 5, finite_special, 'CLP(FD) 0.8.3, 3.1').
runner:case(sys_abs_lin, 5, finite_special, 'CLP(FD) 0.8.3, 3.1, XLOG 1') :-
   call_residue(abs(X) #= Y, L),
   L == [abs(X)#=Y].
runner:case(sys_abs_lin, 5, finite_special, 'CLP(FD) 0.8.3, 3.1, XLOG 2') :-
   call_residue(X #= abs(Y+Z), L),
   L = [_,_,V#=_],
   L == [X#> -1,X#=abs(V),V#=Y+Z].
runner:case(sys_abs_lin, 5, finite_special, 'CLP(FD) 0.8.3, 3.1, XLOG 3') :-
   call_residue(abs(Y)+abs(Z) #= X, L),
   L = [_#=V,_#=W,_],
   L == [abs(Z)#=V,abs(Y)#=W,V#=X-W].
runner:case(sys_abs_lin, 5, finite_special, 'CLP(FD) 0.8.3, 3.1, XLOG 4') :-
   call_residue(Y #= abs(2*X), L),
   L = [_,_,_,V#=_],
   L == [V#> -1,Y#> -1,Y#=2*V,V#=abs(X)].
runner:case(sys_abs_lin, 5, finite_special, 'CLP(FD) 0.8.3, 3.1, XLOG 5') :-
   catch((  abs(X) #= _,
            X = a), error(E,_), true),
   E == type_error(integer,a).

% sys_sqrv(+Wrap, +Wrap)
runner:ref(sys_sqrv, 2, finite_special, 'CLP(FD) 0.8.4, 3.2').
runner:case(sys_sqrv, 2, finite_special, 'CLP(FD) 0.8.4, 3.2, XLOG 1') :-
   call_residue(Y*Y #= X, L),
   L == [Y*Y#=X].
runner:case(sys_sqrv, 2, finite_special, 'CLP(FD) 0.8.4, 3.2, XLOG 2') :-
   call_residue(X*X+Y*Y #= Z, L),
   L = [_#=V,_#=W,_],
   L == [Y*Y#=V,X*X#=W,V#=Z-W].
runner:case(sys_sqrv, 2, finite_special, 'CLP(FD) 0.8.4, 3.2, XLOG 3') :-
   catch((  X*X #= Y,
            Y = a), error(E,_), true),
   E == type_error(integer,a).

% sys_const(+Wrap, +Integer)
% sys_absv(+Wrap, +Wrap)
runner:ref('sys_const sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.3').
runner:case('sys_const sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.3, XLOG 1') :-
   abs(X) #= Y,
   X = 10,
   Y == 10.
runner:case('sys_const sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.3, XLOG 2') :-
   X = -10,
   abs(X) #= Y,
   Y == 10.
runner:case('sys_const sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.3, XLOG 3') :-
   \+ (  Y #= abs(_),
         Y = -10).
runner:case('sys_const sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.3, XLOG 4') :-
   call_residue((  Y #= abs(X),
                   Y = 10), L),
   L == [X in-10\/10].

% sys_var(+Wrap, +Wrap)
% sys_absv(+Wrap, +Wrap)
runner:ref('sys_var sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.4').
runner:case('sys_var sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.4, XLOG 1') :-
   call_residue((  X = Y,
                   abs(X) #= Y), L),
   L == [X#> -1].
runner:case('sys_var sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.4, XLOG 2') :-
   call_residue((  Z #= X+Y,
                   T #= abs(X),
                   Y = 0), L),
   L == [T#> -1,T#=abs(Z)].
runner:case('sys_var sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.4, XLOG 3') :-
   call_residue((  abs(T) #= X,
                   _ #= T+Y,
                   Y = 0), L),
   L == [abs(T)#=X].

% sys_in(+Wrap, +Set, +Bound)
% sys_absv(+Wrap, +Wrap)
% SWI-Prolog usually has stronger bounds
runner:ref('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5').
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 1') :-
   call_residue((  Y #= abs(X),
                   X in -30..20), L),
   L == [X in-30..20,Y in 0..30,Y#=abs(X)].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 2') :-
   call_residue((  abs(X) #= Y,
                   Y in -10..20), L),
   L == [Y in-10..20,X in-20..20,abs(X)#=Y].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 3') :-
   call_residue((  X in 10..20,
                   Y #= abs(X)), L),
   L == [X in 10..20,Y in 10..20,Y#=abs(X)].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 4') :-
   call_residue((  Y #= abs(X),
                   X in -20.. -10), L),
   L == [X in-20.. -10,Y in 10..20,Y#=abs(X)].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 5') :-
   \+ (  abs(_) #= Y,
         Y in -20.. -10).
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 6') :-
   call_residue((  X #> 5,
                   Y #= abs(X)), L),
   L == [X#>5,Y#>5,Y#=abs(X)].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 7') :-
   call_residue((  abs(X) #= Y,
                   Y #> 5), L),
   L == [Y#>5,abs(X)#=Y].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 8') :-
   call_residue((  Y #= abs(X),
                   X #< -5), L),
   L == [X#=< -6,Y#>5,Y#=abs(X)].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 9') :-
   abs(X) #= Y,
   Y #=< 0,
   X == 0.
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 10') :-
   abs(X) #= Y,
   Y in -5..0,
   X == 0.
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 11') :-
   call_residue((  X #> 5,
                   abs(X) #= Y), L),
   L == [Y#>5,X#>5,Y#=abs(X)].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 12') :-
   \+ (  abs(_) #= Y,
         Y #< -5).
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 13') :-
   call_residue((  abs(X) #= Y,
                   Y #=< 5), L),
   L == [Y#=<5,X in-5..5,abs(X)#=Y].
runner:case('sys_in sys_absv', 5, finite_special, 'CLP(FD) 0.8.3, 3.5, XLOG 14') :-
   call_residue((  abs(X) #= Y,
                   Y #\= 0), L),
   L == [Y#\=0,abs(X)#=Y].

% sys_absv(+Wrap, +Wrap)
% sys_absv(+Wrap, +Wrap)
% Not implemented by SWI-Prolog, our implementation is asymmetric
runner:ref('sys_absv sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.6').
runner:case('sys_absv sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.6, XLOG 1') :-
   abs(X) #= Y,
   abs(X) #= Z,
   Y == Z.
runner:case('sys_absv sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.6, XLOG 2') :-
   abs(X) #= Y,
   abs(X) #= 3,
   Y == 3.
runner:case('sys_absv sys_absv', 4, finite_special, 'CLP(FD) 0.8.3, 3.6, XLOG 3') :-
   call_residue((  abs(X) #= 3,
                   abs(X) #= Y), L),
   L = [Y in 0..3,X in-3\/3,Y#=abs(X)].

% sys_const(+Wrap, +Integer)
% sys_sqrv(+Wrap, +Wrap)
runner:ref('sys_const sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.7').
runner:case('sys_const sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.7, XLOG 1') :-
   X = 10,
   X*X #= Y,
   Y == 100.
runner:case('sys_const sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.7, XLOG 2') :-
   call_residue((  X*X #= Y,
                   Y = 100), L),
   L == [X in-10\/10].
runner:case('sys_const sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.7, XLOG 3') :-
   \+ (  Y #= X*X,
         Y = -100).
runner:case('sys_const sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.7, XLOG 4') :-
   \+ (  Y = 101,
         X*X #= Y).

% sys_var(+Wrap, +Wrap)
% sys_sqrv(+Wrap, +Wrap)
runner:ref('sys_var sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.8').
runner:case('sys_var sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.8, XLOG 1') :-
   call_residue((  X*X #= Y,
                   X = Y), L),
   L == [X in 0..1].
runner:case('sys_var sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.8, XLOG 2') :-
   call_residue((  Z #= X+Y,
                   T #= X*X,
                   Y = 0), L),
   L == [T#> -1,T#=Z*Z].
runner:case('sys_var sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.3, 3.8, XLOG 3') :-
   call_residue((  T*T #= X,
                   _ #= T+Y,
                   Y = 0), L),
   L == [T*T#=X].

% sys_in(+Wrap, +Set, +Bound)
% sys_sqrv(+Wrap, +Wrap)
% SWI-Prolog usually has stronger bounds
runner:ref('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9').
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 1') :-
   call_residue((  Y #= X*X,
                   X in -30..20), L),
   L == [X in-30..20,Y in 0..900,Y#=X*X].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 2') :-
   call_residue((  X*X #= Y,
                   Y in -10..20), L),
   L == [Y in-10..20,X in-4..4,X*X#=Y].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 3') :-
   call_residue((  X in 10..20,
                   Y #= X*X), L),
   L == [X in 10..20,Y in 100..400,Y#=X*X].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 4') :-
   call_residue((  Y #= X*X,
                   X in -20.. -10), L),
   L == [X in-20.. -10,Y in 100..400,Y#=X*X].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 5') :-
   \+ (  X*X #= Y,
         Y in -20.. -10).
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 6') :-
   call_residue((  X #> 5,
                   Y #= X*X), L),
   L == [X#>5,Y#>35,Y#=X*X].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 7') :-
   call_residue((  X*X #= Y,
                   Y #> 5), L),
   L == [Y#>5,X*X#=Y].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 8') :-
   call_residue((  Y #= X*X,
                   X #< -5), L),
   L == [X#=< -6,Y#>35,Y#=X*X].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 9') :-
   X*X #= Y,
   Y #=< 0,
   X == 0.
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 10') :-
   X*X #= Y,
   Y in -5..0,
   X == 0.
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 11') :-
   call_residue((  X #> 5,
                   X*X #= Y), L),
   L == [Y#>35,X#>5,Y#=X*X].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 12') :-
   \+ (  X*X #= Y,
         Y #< -5).
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 13') :-
   call_residue((  X*X #= Y,
                   Y #=< 5), L),
   L == [Y#=<5,X in-2..2,X*X#=Y].
runner:case('sys_in sys_sqrv', 5, finite_special, 'CLP(FD) 0.8.3, 3.9, XLOG 14') :-
   call_residue((  X*X #= Y,
                   Y #\= 0), L),
   L == [Y#\=0,X*X#=Y].

% sys_sqrv(+Wrap, +Wrap)
% sys_sqrv(+Wrap, +Wrap)
% Not implemented by SWI-Prolog, our implementation is asymmetric
runner:ref('sys_sqrv sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.4, 3.10').
runner:case('sys_sqrv sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.4, 3.10, XLOG 1') :-
   X*X #= Y,
   X*X #= Z,
   Y == Z.
runner:case('sys_sqrv sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.4, 3.10, XLOG 2') :-
   X*X #= Y,
   X*X #= 4,
   Y == 4.
runner:case('sys_sqrv sys_sqrv', 4, finite_special, 'CLP(FD) 0.8.4, 3.10, XLOG 3') :-
   call_residue((  X*X #= 4,
                   X*X #= Y), L),
   L == [Y in 0..4,X in-2\/2,Y#=X*X].
