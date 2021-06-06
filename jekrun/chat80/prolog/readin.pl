/**
 * Prolog text readin from Chat80 as a module.
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

/**
 * Obtained rights comment in Prolog text and text from LICENSE file:
 *
 * @(#)readin.pl	24.1 2/23/88
 *
 * Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,
 *
 * All Rights Reserved
 *
 * This program may be used, copied, altered or included in other programs
 * only for academic purposes and provided that the authorship of the
 * initial program is acknowledged. Use for commercial purposes without the
 * previous written agreement of the authors is forbidden.
 */

:- module(readin, [read_in/1]).

:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(edinburgh)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(standard/dcg)); true.

/* Read a sentence */

% :- mode initread(-).
% :- mode readrest(+,-).
% :- mode word(-,?,?).
% :- mode words(-,?,?).
% :- mode alphanum(+,-).
% :- mode alphanums(-,?,?).
% :- mode digits(-,?,?).
% :- mode digit(+).
% :- mode lc(+,-).
%
% :- public read_in/1.

/* Read sentence */

read_in(P) :- initread(L), words(P, L, []), !, to_nl.

initread([K1, K2|U]) :- get(K1), get0(K2), readrest(K2, U).

readrest(46, []) :- !.
readrest(63, []) :- !.
readrest(33, []) :- !.
readrest(K, [K1|U]) :- K =< 32, !, get(K1), readrest(K1, U).
readrest(_, [K2|U]) :- get0(K2), readrest(K2, U).

words([V|U]) --> word(V), !, blanks, words(U).
words([]) --> [].

word(U1) --> [K], {lc(K, K1)}, !, alphanums(U2), {name(U1, [K1|U2])}.
word(nb(N)) --> [K], {digit(K)}, !, digits(U), {name(N, [K|U])}.
word(V) --> [K], {name(V, [K])}.

alphanums([K1|U]) --> [K], {alphanum(K, K1)}, !, alphanums(U).
alphanums([]) --> [].

alphanum(95, 95) :- !.
alphanum(K, K1) :- lc(K, K1).
alphanum(K, K) :- digit(K).

digits([K|U]) --> [K], {digit(K)}, !, digits(U).
digits([]) --> [].

blanks --> [K], {K =< 32}, !, blanks.
blanks --> [].

digit(K) :- K > 47, K < 58.

lc(K, K1) :- K > 64, K < 91, !, K1 is K+32.
lc(K, K) :- K > 96, K < 123.

to_nl :-
   repeat,
   get0(10), !.
