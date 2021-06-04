/**
 * Prolog code for the bool test cases.
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
:- use_module(library(experiment/sets)).
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% sys_bool(+Expr, +Expr, +Var)
runner:ref(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1').
/* #<==> */
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 1') :-
   X = 0, X #<==> Y, Y == 0.
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 2') :-
   X #<==> Y, Y = 1, X == 1.
/* #<== */
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 3') :-
   Y = 1, X #<== Y, X == 1.
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 4') :-
   call_residue((X #<== Y, Y = 0), L),
   L == [X in 0..1].
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 5') :-
   X #<== Y, X = 0, Y == 0.
/* #==> */
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 6') :-
   call_residue((Y = 1, X #==> Y), L),
   L == [X in 0..1].
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 7') :-
   X #==> Y, Y = 0, X == 0.
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 8') :-
   X #==> Y, X = 1, Y == 1.
/* #\/ */
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 9') :-
   call_residue((X #\/ Y, Y = 1), L),
   L == [X in 0..1].
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 10') :-
   X = 0, X #\/ Y, Y == 1.
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 11') :-
   _ #\/ Y #\/ _, Y = 1.
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 12') :-
   X #\/ _ #\/ Z, X = 0, Z = 1.
/* #/\ */
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 13') :-
   _ #/\ Y, Y == 1.
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 14') :-
   \+ (X = 0, X #/\ _).
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 15') :-
   catch(#\ foo, error(E, _), true),
   E == type_error(fd_bool, foo).
/* #\ */
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 16') :-
   \+ (X = 1, #\ X).
runner:case(sys_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.1, XLOG 17') :-
   #\ X, X == 0.

% sys_reify_bool(+Expr, +Expr, +Var)
runner:ref(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2').
/* #<==> */
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 1') :-
   call_residue(((X #<==> Y) #<==> B, B = 0), L),
   eq_equal(L, [Y in 0..1, X in 0..1, Y #\= X]).
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 2') :-
   (X #<==> Y) #<==> B, B = 1, X == Y.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 3') :-
   (X #<==> Y) #<==> B, X = 0, Y = 1, B == 0.
/* #<== */
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 4') :-
   (X #<== Y) #<==> B, B = 0, X == 0, Y == 1.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 5') :-
   call_residue(((X #<== Y) #<==> B, B = 1), L),
   eq_equal(L, [Y in 0..1, X in 0..1, Y #=< X]).
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 6') :-
   (X #<== Y) #<==> B, X = 0, Y = 0, B == 1.
/* #==> */
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 7') :-
   (X #==> Y) #<==> B, B = 0, X == 1, Y == 0.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 8') :-
   (X #==> Y) #<==> B, X = 1, Y = 0, B == 0.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 9') :-
   (X #==> Y) #<==> B, X = 0, Y = 1, B == 1.
/* #\/ */
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 10') :-
   call_residue((X #\/ Y #<==> B, B = 1), L),
   eq_equal(L, [Y in 0..1, X in 0..1, Y #\= -X]).
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 11') :-
   X #\/ Y #<==> B, X = 1, Y = 0, B == 1.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 12') :-
   X #\/ Y #<==> B, X = 0, Y = 0, B == 0.
/* #/\ */
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 13') :-
   call_residue((X #/\ Y #<==> B, B = 0), L),
   eq_equal(L, [Y in 0..1, X in 0..1, Y #\= -X+2]).
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 14') :-
   X #/\ Y #<==> B, B = 1, X == 1, Y == 1.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 15') :-
   X #/\ Y #<==> B, X = 0, Y = 1, B == 0.
/* #\ */
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 16') :-
   #\ X #<==> B, B = 0, X == 1.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 17') :-
   #\ X #<==> B, B = 1, X == 0.
runner:case(sys_reify_bool, 3, finite_bool, 'CLP(FD) 0.8.4, 4.2, XLOG 18') :-
   #\ X #<==> B, X = 1, B == 0.

% sys_in(+Wrap, +Set, +Bound)
% sys_reify_bool(+Expr, +Expr, +Var)
runner:ref('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3').
/* #<==> */
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 1') :-
   call_residue((X = 0, (X #<==> Y) #<==> B), L),
   L == [Y in 0..1, Y #= 0 #\/ B #= 0, Y #= 1 #\/ B #= 1, B in 0..1].
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 2') :-
   (X #<==> Y) #<==> B, Y = 1, B == X.
/* #<== */
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 3') :-
   Y = 1, (X #<== Y) #<==> B, B == X.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 4') :-
   (_ #<== Y) #<==> B, Y = 0, B == 1.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 5') :-
   call_residue(((X #<== Y) #<==> B, X = 0), L),
   L == [Y in 0..1, Y #= 0 #\/ B #= 0, Y #= 1 #\/ B #= 1, B in 0..1].
/* #==> */
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 6') :-
   Y = 1, (_ #==> Y) #<==> B, B == 1.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 7') :-
   call_residue(((X #==> Y) #<==> B, Y = 0), L),
   L == [X in 0..1, X #= 0 #\/ B #= 0, X #= 1 #\/ B #= 1, B in 0..1].
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 8') :-
   (X #==> Y) #<==> B, X = 1, B == Y.
/* #\/ */
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 9') :-
   _ #\/ Y #<==> B, Y = 1, B == 1.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 10') :-
   X = 0, X #\/ Y #<==> B, B == Y.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 11') :-
   _ #\/ Y #\/ _ #<==> B, Y = 1, B == 1.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 12') :-
   X #\/ _ #\/ Z #<==> B, X = 0, Z = 1, B == 1.
/* #/\ */
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 13') :-
   X #/\ Y #<==> B, Y = 1, B == X.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 14') :-
   X = 0, X #/\ _ #<==> B, B == 0.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 15') :-
   _ #/\ _ #/\ Z #<==> B, Z = 0, B == 0.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 16') :-
   X #/\ Y #/\ _ #<==> B, Y = 1, X = 0, B == 0.
/* #\ */
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 17') :-
   X = 1, #\ X #<==> B, B == 0.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 18') :-
   #\ X #<==> B, B = 0, X == 1.
runner:case('sys_in sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.3, XLOG 19') :-
   call_residue(#\ X #<==> B, L),
   L == [X in 0..1, X #= 0 #\/ B #= 0, X #= 1 #\/ B #= 1, B in 0..1].

% sys_bool(+Expr, +Expr, +Var)
% sys_reify_bool(+Expr, +Expr, +Var)
runner:ref('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4').
/* #<==> */
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 1') :-
   Y #<==> X, (X #<==> Y) #<==> B, B == 1.
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 2') :-
   (X #<==> Y) #<==> B, #\ (X #<==> Y), B == 0.
/* #<== */
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 3') :-
   Y #<== X, (X #==> Y) #<==> B, B == 1.
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 4') :-
   (X #==> Y) #<==> B, #\ (X #==> Y), B == 0.
/* #==> */
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 5') :-
   (X #<== Y) #<==> B, Y #==> X, B == 1.
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 6') :-
   #\ (Y #==> X), (X #<== Y) #<==> B, B == 0.
/* #\/ */
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 7') :-
   X #\/ Y, X #\/ Y #<==> B, B == 1.
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 8') :-
   X #\/ Y #<==> B, #\ (Y #\/ X), B == 0.
% Not yet
/* #/\ */
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 9') :-
   X #/\ Y #<==> B, Y #/\ X, B == 1.
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 10') :-
   #\ (X #/\ Y), X #/\ Y #<==> B, B == 0.
/* #\ */
runner:case('sys_bool sys_reify_bool', 6, finite_bool, 'CLP(FD) 0.8.4, 4.4, XLOG 11') :-
   #\ X #<==> B, #\ X, B == 1.
