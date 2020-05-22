/**
 * Prolog code for the parallel search example.
 *
 * Perfect numbers were deemed to have important numerological
 * properties by the ancients, and were extensively studied
 * by the Greeks, including Euclid.
 * http://mathworld.wolfram.com/PerfectNumber.html
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

:- use_module(library(advanced/arith)).

perfect(X) :-
   Y is X//2,
   findall(Z, (between(1, Y, Z), X rem Z =:= 0), L),
   sum_list(L, 0, X).

sum_list([], S, S).
sum_list([X|Y], H, S) :-
   J is H+X,
   sum_list(Y, J, S).

%%% single CPU
% ?- use_module(library(advanced/arith)).

% ?- time((between(1,20000,X), perfect(X), fail; true)).
% % Up 10,940 ms, GC 74 ms, Thread Cpu 10,859 ms (Current 03/05/19 02:40:06)
% Yes

%%% multi CPU
% ?- use_module(library(runtime/distributed)).

% ?- time((balance((between(1,20000,X), perfect(X))), fail; true)).
% % Up 6,372 ms, GC 47 ms, Thread Cpu 0 ms (Current 03/04/19 22:35:07)
% Yes