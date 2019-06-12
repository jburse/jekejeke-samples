/**
 * CLP(B) test magic series.
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
;  use_module(library(clpb)).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpb)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(basic/lists)); true.

:- ensure_loaded('../mukai/finsat.p').

% magic(-Binaries)
magic(L) :-
   magic(5, L, B),
   term_variables(L-B, V),
   labeling(V).

% magic(+Integer, -Binaries, -Binaries)
magic(N, L, Z) :-
   length(L, N),
   L ins 0..N,
   series(L, 0, L, Z).

series([], _, _, []).
series([X|R], I, L, [B|Z]) :-
   count(L, I, C, B),
   X #= C,
   J is I+1,
   series(R, J, L, Z).

count([], _, 0, []).
count([X|R], I, B+C, [B|Z]) :-
   B in 0..1,
   X#=I #<==> B,
   count(R, I, C, Z).

