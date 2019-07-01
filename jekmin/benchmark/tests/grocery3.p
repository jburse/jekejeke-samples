/**
 * CLP(FD) code for the modified 7-11 problem.
 *
 * A kid goes into a grocery store and buys three items. The cashier
 * charges $6.42. The kid pays and is about to leave when the cashier
 * calls the kid back, and says "Hold on, I multiplied the three items
 * instead of adding them; I'll try again... Gosh, with adding them
 * the price still comes to $6.42"! What were the prices of the
 * three items?
 *
 * Adaptation of a problem from professor Doug Brumbaugh.
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

:- current_prolog_flag(dialect, jekejeke) -> true
;  use_module(library(clpfd)).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpfd)); true.

% grocery3(-List)
grocery3(X) :-
   X = [A,B,C],
   X ins 0..642,
   A*B*C #= 6420000,
   A+B+C #= 642,
   A #>= B,
   B #>= C,
   label(X).

% Picat test:
% import cp
% X = [A,B,C], X :: 0..642, A*B*C #= 6420000, A+B+C #= 642, A #>= B, B #>= C, time((solve(X))).
% CPU time 0.0 seconds.