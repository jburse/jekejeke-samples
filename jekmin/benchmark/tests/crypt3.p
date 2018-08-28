/**
 * CLP(FD) code for the crypt riddle benchmark.
 *
 * This is problem 223 from:
 * Trigg, W. C. (1985): Mathematical Quickies,
 * Dover Publications, Inc., New York, 1985
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
% :- ensure_loaded('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/ordered/clpfd.p').

% crypt3(X)
crypt3(X) :-
   X = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
   Odd = [A,G,K,M,N],
   Even = [B,C,E,F,H,I,D,J,L,O,P],
   Odd ins 1\/3\/5\/7\/9,
   Even ins 0\/2\/4\/6\/8,
   F #\= 0,
   D #\= 0,
   J #\= 0,
   Z #= A*100+B*10+C,
   Y #= Z*E,
   Y #= F*1000+G*100+H*10+I,
   T #= Z*D,
   T #= J*100+K*10+L,
   10*T+Y #= M*1000+N*100+O*10+P,
   label(X).
