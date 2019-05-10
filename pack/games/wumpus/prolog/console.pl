/**
 * Read line utility based on ISO core standard.
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

:- module(console, [read_line/1]).

/**
 * read_line(X):
 * read_line(S, X):
 * The predicate suceeds in X with a line, not including the end of
 * line characters, or fails if we are at end of file. The binary
 * arity predicate allows specifying an input stream.
 */
% read_line(-Atom)
read_line(X) :-
   current_input(S),
   read_line(S, X).

% read_line(+Stream, -Atom)
read_line(S, X) :-
   get_code(S, C),
   read_line(S, C, L),
   atom_codes(X, L).

% read_line(+Stream, +Code, -List)
read_line(_, -1, _) :- !, fail.
read_line(_, 0'\n, []) :- !.
read_line(S, C, [C|L]) :-
   get_code(S, D),
   read_line2(S, D, L).

% read_line2(+Stream, +Code, -List)
read_line2(_, -1, []) :- !.
read_line2(_, 0'\n, []) :- !.
read_line2(S, C, [C|L]) :-
   get_code(S, D),
   read_line2(S, D, L).
