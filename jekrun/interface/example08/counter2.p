/**
 * Prolog code for the queue example.
 * The Prolog counter.
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

:- use_package(foreign(jekpro/tools/term)).

:- package(library(example08)).
:- module(counter2, []).
:- use_module(foreign('Runnable')).
:- use_module(foreign('Slots')).

:- public new/4.
new(N, Q, W, R) :-
   sys_instance_size(4, R),
   R:set_arg(1, N),
   R:set_arg(2, Q),
   R:set_arg(3, W),
   R:set_arg(4, 0).

:- public run/1.
:- override run/1.
run(R) :-
   T is 'System':currentTimeMillis,
   R:arg(1, N),
   R:arg(2, Q),
   R:arg(3, W),
   R:arg(4, C),
   atom_concat('Process ', N, A1),
   atom_concat(A1, ': ', A2),
   number_codes(C, L),
   atom_codes(A, L),
   atom_concat(A2, A, A3),
   atom_concat(A3, '\n', A4),
   write(W, A4),
   flush_output(W),
   D is C + 1,
   R:set_arg(4, D),
   (  D < 10
   -> S is T + 1000,
      example08/'Queue':post(Q, R, S); true).


