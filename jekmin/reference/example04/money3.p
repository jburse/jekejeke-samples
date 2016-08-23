/**
 * Prolog code for the revisited backtracking example.
 *
 * Puzzle originally published July 1924 issue of
 * Strand Magazine by Henry Dudeney
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

% puzzle(-List)
puzzle(X) :-
   X = [S,E,N,D,M,O,R,Y],
   X ins 0..9,
   all_different(X),
   M #\= 0,
   S #\= 0,
   1000*S+100*E+10*N+D+1000*M+100*O+10*R+E #= 10000*M+1000*O+100*N+10*E+Y,
   label(X).

% ?- puzzle(Z).
% Z = [9,5,6,7,1,0,8,2] ;
% No
