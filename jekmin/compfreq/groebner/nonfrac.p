/**
 * Prolog test cases for the symbolic non fraction.
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

:- use_module(library(groebner/generic)).
:- use_module(library(misc/residue)).

% eval_denest/2
runner:ref(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3').
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich 1') :-
   X is sqrt(3+2*sqrt(2)),
   printable(X, Y),
   Y == 1+sqrt(2).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich 2') :-
   X is sqrt(5+2*sqrt(6)),
   printable(X, Y),
   Y == sqrt(2)+sqrt(3).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich 3') :-
   X is sqrt(sqrt(27))+sqrt(sqrt(12)),
   printable(X, Y),
   Y == sqrt(sqrt(72)+sqrt(75)).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich 4') :-
   X is sqrt(12+2*sqrt(6)+2*sqrt(14)+2*sqrt(21)),
   printable(X, Y),
   Y == sqrt(2)+sqrt(3)+sqrt(7).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich A') :-
   X is sqrt(199999-600*sqrt(111110)),
   printable(X, Y),
   Y == -sqrt(99999)+sqrt(100000).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich 13') :-
   X is sqrt(65-6*sqrt(35)-2*sqrt(22)-6*sqrt(55)+2*sqrt(77)-2*sqrt(14)+6*sqrt(10)),
   printable(X, Y),
   Y == sqrt(2)-sqrt(7)-sqrt(11)+sqrt(45).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich B') :-
   X is sqrt(4+2*sqrt(2)),
   printable(X, Y),
   Y == sqrt(4+sqrt(8)).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich C') :-
   X is sqrt(12+2*sqrt(6)+2*sqrt(14)+2*sqrt(20)),
   printable(X, Y),
   Y == sqrt(12+sqrt(24)+sqrt(56)+sqrt(80)).
runner:case(eval_denest, 2, groebner_nonfrac, 'groebner 0.9.2, 1.3, Jeffrey Rich D') :-
   X is sqrt(5+2*sqrt(6)+5*sqrt(7)+2*sqrt(sqrt(700))+2*sqrt(sqrt(1575))),
   printable(X, Y),
   Y == sqrt(sqrt(175))+sqrt(2)+sqrt(3).

% eval_denest2/2
runner:ref(eval_denest2, 2, groebner_nonfrac, 'groebner 0.9.2, 1.4').
runner:case(eval_denest2, 2, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 1') :-
   X is sqrt(17-sqrt(40)-sqrt(80)+sqrt(200)),
   printable(X, Y),
   Y == -sqrt(2)+sqrt(5)+sqrt(10).
runner:case(eval_denest2, 2, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 2') :-
   X is sqrt(5-sqrt(10)-sqrt(20)+sqrt(50)),
   printable(X, Y),
   Y == sqrt(5-sqrt(10)-sqrt(20)+sqrt(50)).
runner:case(eval_denest2, 2, groebner_nonfrac, 'groebner 0.9.2, 1.4, XLOG 3') :-
   X is sqrt(60-sqrt(480)-sqrt(800)-sqrt(864)+sqrt(1200)-sqrt(1440)+sqrt(2000)+sqrt(2160)),
   printable(X, Y),
   Y == 5-sqrt(2)+sqrt(3)+sqrt(5)-sqrt(10)+sqrt(15).

% eval_denest3/2
runner:ref(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5').
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 2') :-
   X is sqrt(3+2*sqrt(3)),
   printable(X, Y),
   Y == sqrt(3+sqrt(12)).
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 3') :-
   X is sqrt(16-2*sqrt(29)+2*sqrt(55-10*sqrt(29))),
   printable(X, Y),
   Y == sqrt(11-sqrt(116))+sqrt(5).
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 4') :-
   X is sqrt(1+sqrt(3))+sqrt(3+3*sqrt(3))-sqrt(10+6*sqrt(3)),
   printable(X, Y),
   Y == 0.
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 5') :-
   X is sqrt(112+70*sqrt(2)+(46+34*sqrt(2))*sqrt(5)),
   printable(X, Y),
   Y == 5+sqrt(10)+sqrt(32)+sqrt(45).
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 7') :-
   X is sqrt(4+3*sqrt(2)),
   printable(X, Y),
   Y == sqrt(4+sqrt(18)).
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 8') :-
   catch(_ is sqrt(1+sqrt(-1)), error(E,_), true),
   E == evaluation_error(undefined).
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 9') :-
   catch(_ is (1+sqrt(-3))/2, error(E,_), true),
   E == evaluation_error(undefined).
runner:case(eval_denest3, 2, groebner_nonfrac, 'groebner 0.9.2, 1.5, Fagin Hopcroft 10') :-
   X is sqrt(3+sqrt(5+2*sqrt(7))),
   printable(X, Y),
   Y == sqrt(3+sqrt(5+sqrt(28))).
