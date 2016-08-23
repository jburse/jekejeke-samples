/**
 * Prolog code for the clp(FD) record example.
 * The golfer riddle itself.
 *
 * Adapted from Ernest Friedman Hill, 2003
 * Jess in Action: Java Rule-Based Systems
 * Manning Publications Co. Greenwich, CT, USA
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

:- use_module(library(finite/clpfd)).
:- use_module(library(basic/lists)).
:- ensure_loaded(golfer).

solve(Golfers) :-
   Golfers = [Fred,Joe,Bob,Tom],

   new(Fred),
   new(Joe),
   new(Bob),
   new(Tom),

   all_different([position(Fred),position(Joe),position(Bob),position(Tom)]),
   all_different([color(Fred),color(Joe),color(Bob),color(Tom)]),

   position(Joe) #= 2,
   color(Bob) #= 3,                               /* plaid */
   position(Tom) #\= 1,
   position(Tom) #\= 4,
   color(Tom) #\= 4,                              /* orange */

   member(Golfer, Golfers),
   position(Golfer) #= position(Fred)+1,
   color(Golfer) #= 2,                            /* blue */

   term_variables(Golfers, L),
   label(L).

% ?- solve(X).
% X = [golfer(1,4),golfer(2,2),golfer(4,3),golfer(3,1)] ;
% No





