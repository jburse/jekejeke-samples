/**
 * Prolog code for the mutex example.
 * Auto loader variant.
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

init2 :-
   current_output(X),
   assertz(console(X)),
   example06/'Mutex':new(Y),
   assertz(lock(Y)).

exec2(X, Y) :-
   atom_concat('Process ', X, A1),
   atom_concat(A1, ': ', A2),
   atom_concat(A2, Y, A3),
   atom_concat(A3, '\n', A4),
   console(Z),
   write(Z, A4),
   flush_output(Z).

process2(X) :-
   lock(Y), repeat,
   setup_call_cleanup(
      Y::acquire,
      (  exec2(X, 'Ha'),
         exec2(X, 'Tschi')),
      Y::release), fail.

% Window 1
% ?- init2.
% ?- process2('P1').

% Window 2
% ?- process2('P2').