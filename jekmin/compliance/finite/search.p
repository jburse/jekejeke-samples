/**
 * Prolog code for the search test cases.
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

:- use_module(library(finite/clpfd)).
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% indomain/1 finite
runner:ref('indomain finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.1').
runner:case('indomain finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.1, XLOG 1') :-
   indomain(7).
runner:case('indomain finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.1, XLOG 2') :-
   X in 10..15, indomain(X), X == 10.
runner:case('indomain finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.1, XLOG 3') :-
   X in 10..15, indomain(X), X == 11.
runner:case('indomain finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.1, XLOG 4') :-
   X in 10..15, \+ (indomain(X), X == 16).
runner:case('indomain finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.1, XLOG 5') :-
   catch(indomain(foo), error(E, _), true),
   E == type_error(integer, foo).

% indomain/1 infinite
% Not available in SWI-Prolog
runner:ref('indomain infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.2').
runner:case('indomain infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.2, XLOG 1') :-
   indomain(X), (abs(X) > 10 -> !, fail; true), X == -5.
runner:case('indomain infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.2, XLOG 2') :-
   X #\= 0, indomain(X), (abs(X) > 10 -> !, fail; true), X == -5.
runner:case('indomain infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.2, XLOG 3') :-
   X #\= -5, X #\= 0, X #\= 5, \+ (indomain(X), (abs(X) > 10 -> !, fail; true), X == -5).
runner:case('indomain infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.2, XLOG 4') :-
   X #> 5, \+ (indomain(X), (abs(X) > 10 -> !, fail; true), X == -5).
runner:case('indomain infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.2, XLOG 5') :-
   X #=< 5, indomain(X), (abs(X) > 10 -> !, fail; true), X == -5.

% label/1 finite
% SWI-Prolog is stronger, some problems via interval propagation.
runner:ref('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3').
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 1') :-
   label([7]).
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 2') :-
   X in 10..15, label([X]), X == 10.
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 3') :-
   X in 10..15, label([X]), X == 11.
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 4') :-
   X in 10..15, \+ (label([X]), X == 16).
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 5') :-
   [X, Y] ins 0..10, 2*Y #= 3*X, Y #= 5*X-21,
   label([X, Y]), X == 6, Y == 9.
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 6') :-
   [Y, X] ins 0..10, 2*Y #\= 3*X, Y #= 5*X-21,
   \+ (label([X, Y]), X == 6, Y == 9).
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 7') :-
   [Y, X] ins 0..6\/8..11, 2*Y #>= 3*X, Y #=< 5*X-21,
   label([X, Y]), X == 6, Y == 9.
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 8') :-
   [X, Y] ins 0..8\/10..11, 2*Y #>= 3*X, Y #=< 5*X-21,
   \+ (label([X, Y]), X == 6, Y == 9).
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 9') :-
   [Z, Y, X] ins 1..10, X #=< Y, Y #=< Z, 7*(X+Y+Z) #= X*Y*Z,
   label([Z, Y, X]), X == 2, Y == 7, Z == 9.
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 10') :-
   catch(label(_), error(E, _), true),
   E == instantiation_error.
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 11') :-
   catch(label([_|foo]), error(E, _), true),
   E == type_error(list, foo).
runner:case('label finite', 1, finite_search, 'CLP(FD) 0.8.4, 6.3, XLOG 12') :-
   catch(label([foo]), error(E, _), true),
   E == type_error(integer, foo).

% label/1 infinite
% SWI-Prolog is stronger, some problems via interval propagation.
runner:ref('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4').
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 1') :-
   label([X]), (abs(X) > 10 -> !, fail; true), X == -5.
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 2') :-
   X #\= -5, X #\= 5, \+ (label([X]), (abs(X) > 10 -> !, fail; true), X == -5).
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 3') :-
   X #> 5, \+ (label([X]), (abs(X) > 10 -> !, fail; true), X == -5).
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 4') :-
   X #=< 5, label([X]), (abs(X) > 10 -> !, fail; true), X == -5.
% Infinite x Finite
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 5') :-
   X in 0..9, 3*X+5*Y #= 11, label([X, Y]), (abs(Y) > 20 -> !, fail; true), X == 7, Y == -2.
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 6') :-
   Y in 0..9, 3*X+5*Y #= 11, \+ (label([X, Y]), (abs(X) > 20 -> !, fail; true), X == 5, Y == 5).
% Infinite x Infinite
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 5') :-
   3*X+5*Y #= 11, label([X, Y]), (abs(X)+abs(Y) > 20 -> !, fail; true), X == 7, Y == -2.
runner:case('label infinite', 1, finite_search, 'CLP(FD) 0.8.4, 6.4, XLOG 6') :-
   3*X+5*Y #= 11, \+ (label([X, Y]), (abs(X)+abs(Y) > 20 -> !, fail; true), X == 5, Y == 5).

% random_indomain/1
% Not available in SWI-Prolog
runner:ref(random_indomain, 1, finite_search, 'CLP(FD) 1.0.8, 6.5').
runner:case(random_indomain, 1, finite_search, 'CLP(FD) 1.0.8, 6.5, XLOG 1') :-
   random_indomain(7).
runner:case(random_indomain, 1, finite_search, 'CLP(FD) 1.0.8, 6.5, XLOG 2') :-
   X in 10..15, random_indomain(X), X == 10.
runner:case(random_indomain, 1, finite_search, 'CLP(FD) 1.0.8, 6.5, XLOG 3') :-
   X in 10..15, random_indomain(X), X == 11.
runner:case(random_indomain, 1, finite_search, 'CLP(FD) 1.0.8, 6.5, XLOG 4') :-
   X in 10..15, \+ (random_indomain(X), X == 16).
runner:case(random_indomain, 1, finite_search, 'CLP(FD) 1.0.8, 6.5, XLOG 5') :-
   catch(random_indomain(foo), error(E, _), true),
   E == type_error(integer, foo).

% random_label/1
% Not available in SWI-Prolog
runner:ref(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6').
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 1') :-
   random_label([7]).
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 2') :-
   X in 10..15, random_label([X]), X == 10.
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 3') :-
   X in 10..15, random_label([X]), X == 11.
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 4') :-
   X in 10..15, \+ (random_label([X]), X == 16).
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 5') :-
   [X, Y] ins 0..10, 2*Y #= 3*X, Y #= 5*X-21,
   random_label([X, Y]), X == 6, Y == 9.
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 6') :-
   [Y, X] ins 0..10, 2*Y #\= 3*X, Y #= 5*X-21,
   \+ (random_label([X, Y]), X == 6, Y == 9).
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 7') :-
   [Y, X] ins 0..6\/8..11, 2*Y #>= 3*X, Y #=< 5*X-21,
   random_label([X, Y]), X == 6, Y == 9.
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 8') :-
   [X, Y] ins 0..8\/10..11, 2*Y #>= 3*X, Y #=< 5*X-21,
   \+ (random_label([X, Y]), X == 6, Y == 9).
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 9') :-
   [Z, Y, X] ins 1..10, X #=< Y, Y #=< Z, 7*(X+Y+Z) #= X*Y*Z,
   random_label([Z, Y, X]), X == 2, Y == 7, Z == 9.
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 10') :-
   catch(random_label(_), error(E, _), true),
   E == instantiation_error.
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 11') :-
   catch(random_label([_|foo]), error(E, _), true),
   E == type_error(list, foo).
runner:case(random_label, 1, finite_search, 'CLP(FD) 1.0.8, 6.6, XLOG 12') :-
   catch(random_label([foo]), error(E, _), true),
   E == type_error(integer, foo).

% label_maximum/2
% Not available in SWI-Prolog
runner:ref(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7').
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 1') :-
   X in 0..10, M #= -X*X+9*X-14, label_maximum([X], M), M == 6.
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 2') :-
   [X, Y, Z] ins 0..10, X*X+2*Y*Y+3*Z*Z #=< 100, M #= X*Y*Z,
   label_maximum([X, Y, Z], M), X == 6, Y == 4, Z == 3.
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 3a') :-
   findall(X, (X in 0..10, M #= -X*X+9*X-14, label_maximum([X], M)), L),
   L = [R|_], R == 4.
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 3b') :-
   findall(X, (X in 0..10, M #= -X*X+9*X-14, label_maximum([X], M)), L),
   L = [_, R|_], R == 5.
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 3b') :-
   findall(X, (X in 0..10, M #= -X*X+9*X-14, label_maximum([X], M)), L),
   L = [_, _].
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 4') :-
   catch(label_maximum(_, _), error(E, _), true),
   E == instantiation_error.
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 5') :-
   catch(label_maximum([_|foo], _), error(E, _), true),
   E == type_error(list, foo).
runner:case(label_maximum, 2, finite_search, 'CLP(FD) 1.0.8, 6.7, XLOG 6') :-
   catch(label_maximum([foo], _), error(E, _), true),
   E == type_error(integer, foo).
