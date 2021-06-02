/**
 * ECLiPSe Constraint Logic Programming System code for the test harness.
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

% "C:\Program Files\ECLiPSe 7.0\lib\x86_64_nt\eclipse.exe" -L iso

% ?- ensure_loaded('//C/Projects/Jekejeke/Prototyping/samples/jekrun/benchmark/harness/eclipse.p').

uptime(X) :-
   statistics(times, [_,_,T]),
   X is round(T*1000).

gctime(X) :-
   statistics(gc_time, T),
   X is round(T*1000).

:- use_module(library(iso)).
:- op(1150, fx, meta_predicate).
:- get_flag(prolog_suffix, L), append(L, [`.p`], R), set_flag(prolog_suffix, R).

:- ensure_loaded(suite).