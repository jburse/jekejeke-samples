/**
 * DCG parsing of some scalar data types.
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
 * Obtained rights, copyright notice of SWI-Prolog 8.1.6 the
 * Prolog source code basics.pl when we adopted the API specification.
 *
 *  Author:        Jan Wielemaker
 *  E-mail:        J.Wielemaker@vu.nl
 *  WWW:           http://www.swi-prolog.org
 *  Copyright (c)  2012-2016, University of Amsterdam
 *                            VU University Amsterdam
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 *  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */

:- module(dcg_basics, [whites/2,
      
      white/2,
      
      blanks/2,
      
      blank/2,
      
      nonblanks/3,
      
      nonblank/3,
      
      digits/3,
      
      digit/3,
      
      integer/3,
      
      number/3,
      
      xdigits/3,
      
      xdigit/3,
      
      xinteger/3,
      
      string_without/4]).

:- current_prolog_flag(dialect, jekejeke)
-> use_module(unicode); true.
:- current_prolog_flag(dialect, jekejeke)
-> true; use_module(conversion).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(standard/dcg)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(basic/lists)); true.

/**********************************************************/
/* White, Blank and Nonblank                              */
/**********************************************************/

% whites(+List, -List)
whites --> white, !, whites.
whites --> [].

% white(+List, -List)
white --> [C], {code_type(C, white)}.

% blanks(+List, -List)
blanks --> blank, !, blanks.
blanks --> [].

% blank(+List, -List)
blank --> [C], {code_type(C, space)}.

% nonblanks(-List, +List, -List)
nonblanks([H|T]) --> nonblank(H), !, nonblanks(T).
nonblanks([]) --> [].

% nonblank(-Integer, +List, -List)
nonblank(C) --> [C], {code_type(C, graph)}.

/**********************************************************/
/* Digit, Integer and Number                              */
/**********************************************************/

% digits(-List, +List, -List)
digits(L) --> digits(L, []).
digits([H|T], L) --> digit(H), !, digits(T, L).
digits(L, L) --> [].

% digit(-Integer, +List, -List)
digit(C) --> [C], {code_type(C, digit)}.

% integer(-+Integer, +-List, -+List)
integer(I) --> {var(I)}, !, int_codes(L), {number_codes(I, L)}.
integer(I) --> {number_codes(I, L)}, inline(L).

% int_codes(-List, +List, -List)
int_codes(L) --> int_codes(L, []).
int_codes([0'-, D0|D], L) --> "-", !, digit(D0), digits(D, L).
int_codes([D0|D], L) --> "+", !, digit(D0), digits(D, L).
int_codes([D0|D], L) --> digit(D0), digits(D, L).

% inline(+List, -List, +List)
inline([H|T]) --> [H], inline(T).
inline([]) --> [].

% number(-+Number, +-List, -+List)
number(I) --> {var(I)}, !, num_codes(L), {number_codes(I, L)}.
number(I) --> {number_codes(I, L)}, inline(L).

% int_codes(-List, +List, -List)
num_codes(L) --> num_codes(L, []).
num_codes(L, R) --> int_codes(L, H), fraction(H, J), exponent(J, R).

fraction([0'., D0|D], L) --> ".", !, digit(D0), digits(D, L).
fraction(L, L) --> [].

exponent([0'e|L], R) --> "e", !, int_codes(L, R).
exponent([0'e|L], R) --> "E", !, int_codes(L, R).
exponent(L, L) --> [].

/**********************************************************/
/* Hex and String                                         */
/**********************************************************/

% xdigits(-List, +List, -List)
xdigits(L) --> xdigits(L, []).
xdigits([H|T], L) --> xdigit(H), !, xdigits(T, L).
xdigits(L, L) --> [].

% xdigit(-Integer, +List, -List)
xdigit(C) --> [C], {code_type(C, xdigit(_))}.

% xinteger(-+Integer, +-List, -+List)
xinteger(I) --> {var(I)}, !, xint_codes(L), {hex_codes(I, L)}.
xinteger(I) --> {hex_codes(I, L)}, inline(L).

% xint_codes(-List, +List, -List)
xint_codes(L) --> xint_codes(L, []).
xint_codes([0'-, D0|D], L) --> "-", !, xdigit(D0), xdigits(D, L).
xint_codes([D0|D], L) --> "+", !, xdigit(D0), xdigits(D, L).
xint_codes([D0|D], L) --> xdigit(D0), xdigits(D, L).

% string_without(+List, -List, +list, -List)
string_without(Not, L) --> string_without(Not, L, []).
string_without(Not, [H|T], L) --> char_without(Not, H), !, string_without(Not, T, L).
string_without(_, L, L) --> [].

char_without(Not, C) --> [C], {\+ member(C, Not)}.
