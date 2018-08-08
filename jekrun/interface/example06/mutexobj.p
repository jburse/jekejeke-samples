/**
 * Prolog code for the mutex example.
 * Foreign function variant.
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

:- foreign_constructor(new/1, example06/'Mutex', new).

:- virtual acquire/1.
:- foreign(acquire/1, example06/'Mutex', acquire).

:- virtual release/1.
:- foreign(release/1, example06/'Mutex', release).

:- dynamic console/1.
:- dynamic lock/1.

init :-
   current_output(X),
   assertz(console(X)),
   new(Y),
   assertz(lock(Y)).

exec(X, Y) :-
   atom_concat('Process ', X, A1),
   atom_concat(A1, ': ', A2),
   atom_concat(A2, Y, A3),
   atom_concat(A3, '\n', A4),
   console(Z),
   write(Z, A4),
   flush_output(Z).

process(X) :-
   lock(Y), repeat,
   setup_call_cleanup(
      acquire(Y),
      (  exec(X, 'Ha'),
         exec(X, 'Tschi')),
      release(Y)), fail.

% Window 1
% ?- init.
% ?- process('P1').

% Window 2
% ?- process('P2').
