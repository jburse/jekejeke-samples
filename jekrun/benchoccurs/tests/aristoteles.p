/**
 * Convert Term Logic into First Order Logic
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information.XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH.If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document.Reproduction
 * of the information is only allowed for non-commercial uses.Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library.Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

% :- encoding(utf8).

:- ensure_loaded(prepare).
:- ensure_loaded(koutsoukou).

% form(+Form, -Form)
form(A∈B, P) :-
   P =.. [B, A].
form(A a B, all(X, (P -: Q))) :-
   P =.. [A, X],
   Q =.. [B, X].
form(A e B, all(X, (P -: -Q))) :-
   P =.. [A, X],
   Q =.. [B, X].
form(A i B, exist(X, (P, Q))) :-
   P =.. [A, X],
   Q =.. [B, X].
form(A o B, exist(X, (P, -Q))) :-
   P =.. [A, X],
   Q =.. [B, X].
form(n A, exist(X, P)) :-
   P =.. [A, X].
form((A -: B), (C -: D)) :-
   form(A, C),
   form(B, D).
form((A, B), (C, D)) :-
   form(A, C),
   form(B, D).
form(-A, -B) :-
   form(A, B).
